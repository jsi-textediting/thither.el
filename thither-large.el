;;; thither-large.el --- Large-file sliding-viewport subsystem for thither  -*- lexical-binding: t; -*-

;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (thither "0.1"))
;; Keywords: files, comm, tools

;;; Commentary:

;; Sliding viewport editing and navigation of multi-gigabyte remote files
;; over thither-server without pulling full file content into Emacs memory.
;;
;; Provides:
;;   - O(1) bounded memory usage via sliding window buffer (`thither-large-file-mode')
;;   - O(log K) newline binary search lookups for fast line navigation
;;   - Server-side PCRE2 and literal search with clickable results buffer
;;   - In-viewport editing with atomic differential saving via `apply_edit'
;;   - Automatic threshold routing on `find-file'

;;; Code:

(require 'cl-lib)
(require 'thither)
(require 'thither-fs)

(defgroup thither-large nil
  "Large file editing and navigation for thither remote files."
  :group 'thither)

(defcustom thither-large-file-threshold (* 10 1024 1024)
  "File size threshold in bytes to activate `thither-large-file-mode'.
Files equal to or exceeding this size (default 10 MB) will open in
large-file viewport mode instead of being fully downloaded."
  :type 'integer
  :group 'thither-large)

(defcustom thither-large-window-lines 5000
  "Default number of lines loaded into the sliding viewport."
  :type 'integer
  :group 'thither-large)

(defcustom thither-large-window-max-bytes (* 4 1024 1024)
  "Most bytes loaded into the sliding viewport.
The window ends before `thither-large-window-lines' lines when they are
long.  A single line longer than this is shown only in part, read-only."
  :type 'integer
  :group 'thither-large)

(defcustom thither-large-confirm t
  "Whether to ask confirmation before opening a file in large-file mode.
If nil, automatically use `thither-large-file-mode' when size exceeds
`thither-large-file-threshold'."
  :type 'boolean
  :group 'thither-large)

(defcustom thither-large-chunk-size 65536
  "Default chunk size in bytes for the remote line index."
  :type 'integer
  :group 'thither-large)

(defcustom thither-large-edit-rate (* 20 1024 1024)
  "Bytes per second the host is assumed to copy at least when saving.
A save rewrites the whole file on the host, so its timeout is
`thither-timeout' (at least 60 s) plus the file size at this rate."
  :type 'integer
  :group 'thither-large)

(defcustom thither-large-search-limit 1000
  "Most matches one remote search returns (the server allows 100000).
`thither-large-search-more' fetches the next ones."
  :type 'integer
  :group 'thither-large)

;;;; Buffer-local variables

(defvar-local thither-large-host nil
  "Host of the current large remote file.")
(defvar-local thither-large-path nil
  "Host path of the current large remote file.")
(defvar-local thither-large-file-name nil
  "The file name the viewport belongs to.
When `buffer-file-name' differs (after `write-file'), a save is a save-as.")
(defvar-local thither-large-etag nil
  "Etag of the current large remote file.")
(defvar-local thither-large-size 0
  "Total byte size of the remote file.")
(defvar-local thither-large-chunks nil
  "Vector of [LEN LF] chunk descriptors from `thither-lineindex'.")
(defvar-local thither-large-ends-with-nl nil
  "Whether the remote file ends with a newline character.")
(defvar-local thither-large-chunk-byte-offsets nil
  "Vector of cumulative byte offsets at the start of each chunk.")
(defvar-local thither-large-chunk-line-offsets nil
  "Vector of cumulative 1-based start line numbers at the start of each chunk.")
(defvar-local thither-large-total-lines 0
  "Total number of lines in the remote file.")
(defvar-local thither-large-window-start-line 1
  "1-based starting line of the active viewport window.")
(defvar-local thither-large-window-end-line 1
  "1-based ending line of the active viewport window.")
(defvar-local thither-large-window-start-byte 0
  "0-based starting byte offset of the active viewport window.")
(defvar-local thither-large-window-end-byte 0
  "0-based ending byte offset (exclusive) of the active viewport window.")
(defvar-local thither-large-window-orig-text ""
  "Original raw bytes corresponding to the current viewport window.")
(defvar-local thither-large-window-truncated nil
  "Non-nil when the window holds only the beginning of one overlong line.
Such a window is read-only and cannot be saved.")
(defvar-local thither-large--forced-read-only nil
  "Non-nil when the buffer is read-only only because the window is truncated.")
(defvar-local thither-large-coding-system 'utf-8
  "Coding system for decoding the viewport text.
Its end-of-line type is detected per window unless it has one.")
(defvar-local thither-large-window-coding nil
  "Coding system that encodes the window text back to its original bytes.")
(defvar-local thither-large--search-matches nil
  "Vector of recent search match hash tables.")
(defvar-local thither-large--search-index 0
  "Index of currently selected search match.")
(defvar-local thither-large--search-pattern nil
  "Pattern from the most recent remote search.")
(defvar-local thither-large--search-regex nil
  "Whether the most recent remote search used a regular expression.")
(defvar-local thither-large--search-case t
  "Whether the most recent remote search was case-sensitive.")
(defvar-local thither-large--search-more nil
  "Non-nil when the most recent remote search stopped at its limit.")
(defvar-local thither-large--pending-init nil
  "Internal flag indicating `thither-large-file-mode' should be established.")

;; Preserve state across major-mode resets (e.g. from `normal-mode')
(dolist (v '(thither-large-host
             thither-large-path
             thither-large-file-name
             thither-large-etag
             thither-large-size
             thither-large-chunks
             thither-large-ends-with-nl
             thither-large-chunk-size
             thither-large-window-lines
             thither-large-chunk-byte-offsets
             thither-large-chunk-line-offsets
             thither-large-total-lines
             thither-large-window-start-line
             thither-large-window-end-line
             thither-large-window-start-byte
             thither-large-window-end-byte
             thither-large-window-orig-text
             thither-large-window-truncated
             thither-large--forced-read-only
             thither-large-coding-system
             thither-large-window-coding
             thither-large--search-matches
             thither-large--search-index
             thither-large--search-pattern
             thither-large--search-regex
             thither-large--search-case
             thither-large--search-more
             thither-large--pending-init))
  (put v 'permanent-local t))

;;;; Mathematics & Index Lookups

(defun thither-large--compute-prefix-sums ()
  "Compute cumulative byte and line offsets from `thither-large-chunks'."
  (let* ((chunks (or thither-large-chunks []))
         (n (length chunks))
         (byte-offsets (make-vector (1+ n) 0))
         (line-offsets (make-vector (1+ n) 1)))
    (aset byte-offsets 0 0)
    (aset line-offsets 0 1)
    (dotimes (i n)
      (let* ((c (aref chunks i))
             (chunk-len (aref c 0))
             (chunk-lf (aref c 1)))
        (aset byte-offsets (1+ i) (+ (aref byte-offsets i) chunk-len))
        (aset line-offsets (1+ i) (+ (aref line-offsets i) chunk-lf))))
    (setq thither-large-chunk-byte-offsets byte-offsets
          thither-large-chunk-line-offsets line-offsets)
    (let ((total-lf (1- (aref line-offsets n))))
      (setq thither-large-total-lines
            (cond ((= thither-large-size 0) 0)
                  (thither-large-ends-with-nl total-lf)
                  (t (1+ total-lf)))))))

(defun thither-large--set-index (idx)
  "Take size, chunks and ends_with_nl from the index hash table IDX."
  (setq thither-large-etag (gethash "etag" idx)
        thither-large-size (gethash "size" idx)
        thither-large-chunks (gethash "chunks" idx)
        thither-large-ends-with-nl (thither-true-p (gethash "ends_with_nl" idx)))
  (thither-large--compute-prefix-sums))

(defun thither-large--get-chunk-for-newline (target-nl)
  "Binary search for the 0-based chunk index containing newline TARGET-NL."
  (let* ((n (length thither-large-chunks))
         (low 0)
         (high (1- n))
         (ans (1- n)))
    (while (<= low high)
      (let* ((mid (/ (+ low high) 2))
             (before (1- (aref thither-large-chunk-line-offsets mid)))
             (chunk-lf (aref (aref thither-large-chunks mid) 1))
             (total-in (+ before chunk-lf)))
        (cond
         ((<= target-nl before)
          (setq high (1- mid)))
         ((> target-nl total-in)
          (setq low (1+ mid)))
         (t
          (setq ans mid
                low (1+ high))))))
    ans))

(defun thither-large--get-chunk-for-byte (byte)
  "Binary search for the 0-based index of the chunk holding byte offset BYTE."
  (let ((low 0) (high (1- (length thither-large-chunks))))
    ;; the last chunk whose start is <= BYTE
    (while (< low high)
      (let ((mid (/ (+ low high 1) 2)))
        (if (<= (aref thither-large-chunk-byte-offsets mid) byte)
            (setq low mid)
          (setq high (1- mid)))))
    low))

(defun thither-large--read (off len)
  "Read LEN bytes at byte offset OFF of the remote file."
  (if (> len 0)
      (thither-read-range (thither-connection thither-large-host) thither-large-path
                          off len thither-large-etag)
    ""))

(defun thither-large--line-to-byte-offset (line)
  "Resolve exact 0-based byte offset for 1-based LINE.
Line 1 starts at byte 0.
Line L (L > 1) starts immediately after (L - 1)-th newline."
  (cond
   ((<= line 1) 0)
   ((> line thither-large-total-lines) thither-large-size)
   (t
    (let* ((target-nl (1- line))
           (k (thither-large--get-chunk-for-newline target-nl))
           (before-nl (1- (aref thither-large-chunk-line-offsets k)))
           (need-in-chunk (- target-nl before-nl))
           (byte-start (aref thither-large-chunk-byte-offsets k))
           (chunk-len (aref (aref thither-large-chunks k) 0))
           (data (thither-large--read byte-start chunk-len)))
      (if (or (null data) (not (stringp data)))
          byte-start
        (let ((count 0)
              (pos 0)
              (len (length data))
              (found nil))
          (while (and (< pos len) (not found))
            (when (= (aref data pos) ?\n)
              (setq count (1+ count))
              (when (= count need-in-chunk)
                (setq found (1+ pos))))
            (setq pos (1+ pos)))
          (if found
              (+ byte-start found)
            (+ byte-start len))))))))

(defun thither-large--byte-to-line (byte)
  "Return the 1-based line that holds byte offset BYTE."
  (if (or (null thither-large-chunks) (= (length thither-large-chunks) 0))
      1
    (let* ((byte (max 0 (min byte thither-large-size)))
           (k (thither-large--get-chunk-for-byte byte))
           (start (aref thither-large-chunk-byte-offsets k))
           (data (thither-large--read start (- byte start))))
      (+ (aref thither-large-chunk-line-offsets k) (cl-count ?\n data)))))

(defun thither-large--line-range-to-byte-range (start-line end-line)
  "Resolve byte range (START-BYTE . END-BYTE) for lines [START-LINE, END-LINE]."
  (let ((start-byte (thither-large--line-to-byte-offset start-line))
        (end-byte (if (>= end-line thither-large-total-lines)
                      thither-large-size
                    (thither-large--line-to-byte-offset (1+ end-line)))))
    (cons start-byte end-byte)))

;;;; Coding

(defun thither-large--round-trip-p (text coding raw)
  (string= (encode-coding-string text coding t) raw))

(defun thither-large--decode (raw)
  "Decode the window bytes RAW.  Return (TEXT . CODING).
Encoding TEXT with CODING gives RAW back, so an unchanged window saves
byte for byte: CRLF files keep their CRs, and windows with mixed line
ends keep the CRs as ^M characters."
  (let* ((base (or thither-large-coding-system 'utf-8))
         (text (decode-coding-string raw base))
         (coding last-coding-system-used))
    ;; no line end seen: do not encode with the system's default one
    (when (vectorp (coding-system-eol-type coding))
      (setq coding (coding-system-change-eol-conversion coding 'unix)))
    (if (thither-large--round-trip-p text coding raw)
        (cons text coding)
      (let* ((unix (coding-system-change-eol-conversion base 'unix))
             (text (decode-coding-string raw unix)))
        (if (thither-large--round-trip-p text unix raw)
            (cons text unix)
          (cons (decode-coding-string raw 'raw-text-unix) 'raw-text-unix))))))

;;;; Viewport Window Management

(defun thither-large-header-string ()
  "Generate status string for the viewport header line."
  (let ((pct (if (> thither-large-total-lines 0)
                 (* 100.0 (/ (float thither-large-window-start-line)
                             (float thither-large-total-lines)))
               0.0)))
    (format " Thither Large: %s | %s | Lines %s-%s of %s (%.1f%%)%s | [n/p] page, [g] goto, [s] search "
            (file-name-nondirectory (or buffer-file-name thither-large-path ""))
            (file-size-human-readable thither-large-size)
            thither-large-window-start-line
            thither-large-window-end-line
            thither-large-total-lines
            pct
            (if thither-large-window-truncated " TRUNCATED LINE, read-only" ""))))

(defun thither-large--update-read-only ()
  "Make the buffer read-only while the window is truncated."
  (cond ((and thither-large-window-truncated (not buffer-read-only))
         (setq buffer-read-only t
               thither-large--forced-read-only t))
        ((and (not thither-large-window-truncated) thither-large--forced-read-only)
         (setq buffer-read-only nil
               thither-large--forced-read-only nil))))

(defun thither-large-load-window (start-line end-line)
  "Load lines [START-LINE, END-LINE] into the buffer.
At most `thither-large-window-max-bytes' are loaded: the window ends
earlier when the lines are long.  The load is not recorded for undo."
  (let ((no-undo (eq buffer-undo-list t))
        (start-byte 0) (end-byte 0) (raw "") truncated)
    (if (<= thither-large-total-lines 0)
        (setq start-line 1 end-line 1)
      (setq start-line (max 1 (min start-line thither-large-total-lines)))
      (setq end-line (max start-line (min end-line thither-large-total-lines)))
      (let ((range (thither-large--line-range-to-byte-range start-line end-line))
            (cap (max 1 (or thither-large-window-max-bytes (* 4 1024 1024)))))
        (setq start-byte (car range)
              end-byte (cdr range))
        (when (> (- end-byte start-byte) cap)
          (let ((line (thither-large--byte-to-line (+ start-byte cap))))
            (if (> line start-line)
                ;; lines [START-LINE, LINE - 1] end at or before the cap
                (setq end-line (1- line)
                      end-byte (thither-large--line-to-byte-offset line))
              (setq end-line start-line
                    end-byte (+ start-byte cap)
                    truncated t))))
        (setq raw (thither-large--read start-byte (- end-byte start-byte)))))
    (let ((dec (thither-large--decode raw))
          (inhibit-read-only t))
      (let ((buffer-undo-list t))
        (erase-buffer)
        (insert (car dec)))
      ;; undo must not bring back the text of another window
      (unless no-undo (setq buffer-undo-list nil))
      (setq thither-large-window-start-line start-line
            thither-large-window-end-line end-line
            thither-large-window-start-byte start-byte
            thither-large-window-end-byte end-byte
            thither-large-window-orig-text raw
            thither-large-window-truncated truncated
            thither-large-window-coding (cdr dec)
            buffer-file-coding-system (cdr dec))
      (thither-large--update-read-only)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (force-mode-line-update))))

;;;; Differential Saving

(defun thither-large--edit-script ()
  "The `apply_edit' script that replaces the window by insert 1."
  (vconcat
   (and (> thither-large-window-start-byte 0)
        (list `(("keep" . t) ("off" . 0) ("len" . ,thither-large-window-start-byte))))
   (list '(("ins" . 1)))
   (and (< thither-large-window-end-byte thither-large-size)
        (list `(("keep" . t)
                ("off" . ,thither-large-window-end-byte)
                ("len" . ,(- thither-large-size thither-large-window-end-byte)))))))

(defun thither-large--encode-window ()
  "The window text encoded as it will be saved."
  (let* ((coding (or coding-system-for-write thither-large-window-coding 'utf-8-unix))
         (bad (unencodable-char-position (point-min) (point-max) coding)))
    (when bad
      (goto-char bad)
      (user-error "Character `%c' at %d cannot be encoded with %s" (char-after bad) bad coding))
    (encode-coding-string (buffer-string) coding t)))

(defun thither-large--edit-timeout ()
  "Seconds a save may take: the host copies the whole file."
  (+ (max thither-timeout 60)
     (/ thither-large-size (max 1 thither-large-edit-rate))))

(defun thither-large--timed-out (conn path file before)
  "Report a save of the window to PATH (FILE) that timed out.
BEFORE is the etag PATH had (\"-\": none).  The request was cancelled,
but it may have finished first: look at the file on the host.  The buffer
keeps its etag, so a further save is a conflict rather than an overwrite."
  (let* ((st (ignore-errors (thither-stat conn path)))
         (now (if st (gethash "etag" st) "-")))
    (if (and before (equal now before))
        (error "Saving %s timed out; the host file is unchanged, nothing was written" file)
      (error "Saving %s timed out, and the host file has changed (etag %s): the save may \
have completed.  Revert the buffer to see the file as it is now" file now))))

(defun thither-large--apply (encoded &optional dest dest-if-match file)
  "Replace the window by ENCODED with `apply_edit'; return the new index.
With DEST write the result there (DEST-IF-MATCH as for `thither-apply-edit').
FILE names the target in messages."
  (let ((conn (thither-connection thither-large-host))
        (file (or file buffer-file-name thither-large-path)))
    (condition-case err
        (thither-apply-edit conn thither-large-path thither-large-etag
                            (thither-large--edit-script) (vector encoded)
                            :chunk-size thither-large-chunk-size
                            :dest dest :dest-if-match dest-if-match
                            :timeout (thither-large--edit-timeout))
      (thither-error
       (let ((code (cadr err)) (extra (cadddr err)))
         (cond
          ((equal code "conflict")
           (error "Conflict! %s changed on host (etag: %s). Save aborted"
                  (if dest file "Remote file")
                  (and (hash-table-p extra) (gethash "etag" extra))))
          ((and dest (equal code "EEXIST"))
           (error "%s was created on the host meanwhile. Save aborted" file))
          ((equal code "timeout")
           (thither-large--timed-out conn (or dest thither-large-path) file
                                     (if dest dest-if-match thither-large-etag)))
          (t (signal (car err) (cdr err)))))))))

(defun thither-large--saved (res encoded)
  "Record the index RES of the file just saved with the window ENCODED."
  (thither-large--set-index res)
  (setq thither-large-window-orig-text encoded
        thither-large-window-end-byte (+ thither-large-window-start-byte (length encoded)))
  (thither--set-etag buffer-file-name thither-large-etag)
  (let ((nl-count (count-matches "\n" (point-min) (point-max))))
    (setq thither-large-window-end-line
          (+ thither-large-window-start-line
             (if (and (> (buffer-size) 0)
                      (/= (char-after (1- (point-max))) ?\n))
                 nl-count
               (max 0 (1- nl-count))))))
  (set-buffer-modified-p nil)
  (force-mode-line-update)
  (message "Wrote %s (size: %s, etag: %s)"
           (or buffer-file-name thither-large-path)
           (file-size-human-readable thither-large-size)
           thither-large-etag)
  t)

(defun thither-large--save-in-place ()
  "Save the window into the file it was loaded from."
  (if (not (buffer-modified-p))
      (message "(No changes need to be saved)")
    (let ((encoded (thither-large--encode-window)))
      (if (string= encoded thither-large-window-orig-text)
          (progn
            (set-buffer-modified-p nil)
            (message "(No changes need to be saved)"))
        (thither-large--saved (thither-large--apply encoded) encoded))))
  t)

(defun thither-large--restore-name ()
  "Make the buffer visit `thither-large-file-name' again."
  (let ((change-major-mode-with-file-name nil))
    (set-visited-file-name thither-large-file-name t))
  (thither--set-etag buffer-file-name thither-large-etag))

(defun thither-large--save-as (name)
  "Write the file with the edited window to NAME (after `write-file').
Only possible on the same host, with `apply_edit' to a destination: the
rest of the file never leaves the host.  Otherwise the buffer is made to
visit its file again and the save is refused, so that neither the original
file is changed nor the viewport alone written as the new file."
  (let* ((target (and (thither--split name) (thither--path name)))
         (conn (thither-connection thither-large-host)))
    (cond
     ((and target (equal (car target) thither-large-host)
           (equal (cdr target) thither-large-path))
      ;; the same file under another name
      (setq thither-large-file-name name)
      (thither-large--save-in-place))
     ((not (and target (equal (car target) thither-large-host)
                (thither-has-cap conn "apply_edit_dest")))
      (thither-large--restore-name)
      (user-error "%s"
                  (if (and target (equal (car target) thither-large-host))
                      "thither-server on the host is too old to save a large file under another name; upgrade it"
                    (format "A large-file buffer can only be written to another file on host %s"
                            thither-large-host))))
     (t
      (let* ((encoded (thither-large--encode-window))
             (st (thither-stat conn (cdr target)))
             (if-match (if st (gethash "etag" st) "-"))
             (res (thither-large--apply encoded (cdr target) if-match name)))
        (setq thither-large-path (cdr target)
              thither-large-file-name name)
        (thither-large--saved res encoded))))))

(defun thither-large-save-buffer ()
  "Save the current viewport back to the remote file via `thither-apply-edit'.
After `write-file' this writes the whole file under the new name."
  (interactive)
  (when thither-large-window-truncated
    (user-error "The window holds only part of a long line; it cannot be saved"))
  (if (and thither-large-file-name buffer-file-name
           (not (equal buffer-file-name thither-large-file-name)))
      (thither-large--save-as buffer-file-name)
    (thither-large--save-in-place)))

(defun thither-large--keep-save-hook ()
  "In `before-save-hook': save viewport buffers by `thither-large-save-buffer'.
A change of major mode (e.g. by `set-visited-file-name' in `write-file')
drops the buffer-local `write-contents-functions', and the ordinary write
would store the viewport alone as the whole file."
  (when thither-large-host
    (add-hook 'write-contents-functions #'thither-large-save-buffer nil t)))

(add-hook 'before-save-hook #'thither-large--keep-save-hook)

;;;; Navigation Commands

(defun thither-large--check-modified ()
  "Before the window moves: save the changes or refuse to move."
  (when (buffer-modified-p)
    (if (y-or-n-p "Buffer has unsaved changes. Save before moving viewport? ")
        (thither-large-save-buffer)
      (user-error "Aborted viewport move due to unsaved changes"))))

(defun thither-large--show (start)
  "Load the window of `thither-large-window-lines' lines from line START.
Near the end of the file the window starts earlier, so it stays full."
  (let* ((win (max 1 (or thither-large-window-lines 5000)))
         (start (max 1 (min start (- thither-large-total-lines win -1)))))
    (thither-large-load-window start (+ start win -1))))

(defun thither-large--move-to-byte-column (col)
  "Move point from the start of its line over COL - 1 bytes of the line.
COL is a 1-based byte column as the server reports it (0: unknown)."
  (when (and col (> col 1))
    (let* ((coding (or thither-large-window-coding 'utf-8-unix))
           (bytes (encode-coding-string
                   (buffer-substring-no-properties (point) (line-end-position)) coding t))
           (prefix (substring bytes 0 (min (1- col) (length bytes)))))
      (forward-char (min (length (decode-coding-string prefix coding))
                         (- (line-end-position) (point)))))))

(defun thither-large-goto-line (line &optional col)
  "Move the large-file viewport so LINE is visible, and place point there.
COL is an optional 1-based byte column, as in search results."
  (interactive "nGo to line: ")
  (thither-large--check-modified)
  (setq line (max 1 (min line (max 1 thither-large-total-lines))))
  (let ((half (/ (max 1 (or thither-large-window-lines 5000)) 2)))
    (thither-large--show (- line half))
    ;; long lines made the window end before LINE: start it there
    (when (> line thither-large-window-end-line)
      (thither-large-load-window line (+ line (max 1 (or thither-large-window-lines 5000)) -1)))
    (goto-char (point-min))
    (forward-line (- line thither-large-window-start-line))
    (thither-large--move-to-byte-column col)))

(defun thither-large-goto-percent (pct)
  "Move the viewport to PCT (0-100) percent of the file."
  (interactive "nGo to percent (0-100): ")
  (if (<= thither-large-total-lines 0)
      (thither-large-goto-line 1)
    (let ((target (max 1 (floor (* thither-large-total-lines (/ (float pct) 100.0))))))
      (thither-large-goto-line target))))

(defun thither-large--page-step ()
  "Lines to page by: 75% of the window, so a quarter stays visible."
  (max 1 (floor (* (or thither-large-window-lines 5000) 0.75))))

(defun thither-large--page-to (start)
  "Load the window of `thither-large-window-lines' lines from line START."
  (thither-large-load-window start (+ start (max 1 (or thither-large-window-lines 5000)) -1)))

(defun thither-large-page-next ()
  "Scroll the viewport forward by ~75% of window size."
  (interactive)
  (if (>= thither-large-window-end-line thither-large-total-lines)
      (message "End of file")
    (thither-large--check-modified)
    ;; long lines may have ended the window early: do not skip any
    (thither-large--page-to (min (+ thither-large-window-start-line (thither-large--page-step))
                                 (1+ thither-large-window-end-line)))))

(defun thither-large-page-prev ()
  "Scroll the viewport backward by ~75% of window size."
  (interactive)
  (if (<= thither-large-window-start-line 1)
      (message "Beginning of file")
    (thither-large--check-modified)
    (thither-large--page-to (max 1 (- thither-large-window-start-line (thither-large--page-step))))))

(defun thither-large-head ()
  "Jump viewport to the beginning of the file."
  (interactive)
  (thither-large-goto-line 1))

(defun thither-large-tail ()
  "Jump viewport to the end of the file."
  (interactive)
  (thither-large-goto-line (max 1 thither-large-total-lines)))

;;;; Search Integration

(defvar-local thither-search--source nil
  "The large-file buffer the results in this buffer belong to.")

(defvar thither-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'thither-search-goto-match)
    (define-key map [mouse-2] #'thither-search-goto-match)
    (define-key map (kbd "m") #'thither-search-more)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `thither-search-mode'.")

(define-derived-mode thither-search-mode special-mode "Thither-Search"
  "Major mode for displaying thither server-side search results."
  (setq buffer-read-only t))

(defun thither-search-goto-match ()
  "Jump to the search match on current line in source buffer."
  (interactive)
  (let ((m (get-text-property (point) 'thither-search-match))
        (src (get-text-property (point) 'thither-source-buffer)))
    (unless (and m (buffer-live-p src))
      (user-error "No valid match at point"))
    (let ((line (gethash "line" m))
          (col (gethash "col" m)))
      (pop-to-buffer src)
      (thither-large-goto-line line col))))

(defun thither-search-more ()
  "Fetch the next search results of the source buffer."
  (interactive)
  (unless (buffer-live-p thither-search--source)
    (user-error "The searched buffer is gone"))
  (with-current-buffer thither-search--source
    (thither-large-search-more)))

(defconst thither-large--search-server-max 100000
  "Most matches the server returns for one search.")

(defun thither-large--search-batch (from-off)
  "Run the current search from byte FROM-OFF; return the matches.
One match more than `thither-large-search-limit' is asked for, so that
`thither-large--search-more' is set only when more matches exist."
  (let* ((limit (max 1 (min thither-large-search-limit thither-large--search-server-max)))
         (ask (min (1+ limit) thither-large--search-server-max))
         (matches (or (thither-search (thither-connection thither-large-host) thither-large-path
                                      thither-large--search-pattern
                                      :regex thither-large--search-regex
                                      :case thither-large--search-case
                                      :limit ask
                                      :from-off from-off
                                      :etag thither-large-etag)
                      [])))
    (setq thither-large--search-more (>= (length matches) ask))
    (if (> (length matches) limit) (cl-subseq matches 0 limit) matches)))

(defun thither-large--search-show ()
  "Fill *thither-search* with the matches of the current buffer."
  (let ((search-buf (get-buffer-create "*thither-search*"))
        (src-buf (current-buffer))
        (matches thither-large--search-matches)
        (pattern thither-large--search-pattern)
        (more thither-large--search-more))
    (with-current-buffer search-buf
      (unless (derived-mode-p 'thither-search-mode)
        (thither-search-mode))
      (setq thither-search--source src-buf)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%d matches for \"%s\" in %s%s:\n\n"
                        (length matches) pattern (buffer-name src-buf)
                        (if more " (limit reached, `m' fetches more)" "")))
        (dotimes (i (length matches))
          (let* ((m (aref matches i))
                 (start-pos (point)))
            (insert (format "Line %d, Col %d (byte %d, len %d)\n"
                            (gethash "line" m) (gethash "col" m)
                            (gethash "off" m) (gethash "len" m)))
            (add-text-properties start-pos (point)
                                 (list 'thither-search-match m
                                       'thither-source-buffer src-buf
                                       'mouse-face 'highlight
                                       'help-echo "mouse-2, RET: jump to match"))))
        (goto-char (point-min))
        (forward-line 2)))
    (display-buffer search-buf)))

(defun thither-large--limit-note ()
  (if thither-large--search-more
      (format " (first %d only; M-x thither-large-search-more for more)"
              (length thither-large--search-matches))
    ""))

(defun thither-large-search (pattern &optional regex case)
  "Search the entire remote file for PATTERN using `thither-search'.
The search is literal and case-sensitive; with prefix argument, prompt for
REGEX and CASE sensitivity."
  (interactive
   (let* ((def (or (and (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                   (thing-at-point 'symbol)
                   ""))
          (prompt (if (string-empty-p def) "Search pattern: " (format "Search pattern (default %s): " def)))
          (pat (read-string prompt nil nil def)))
     (if current-prefix-arg
         (list pat (y-or-n-p "Regular expression? ") (y-or-n-p "Case-sensitive? "))
       (list pat nil t))))
  (setq thither-large--search-pattern pattern
        thither-large--search-regex regex
        thither-large--search-case case)
  (let ((matches (thither-large--search-batch 0)))
    (if (= (length matches) 0)
        (message "No matches found for: %s" pattern)
      (setq thither-large--search-matches matches
            thither-large--search-index 0)
      (thither-large--search-show)
      (let ((first-m (aref matches 0)))
        (thither-large-goto-line (gethash "line" first-m) (gethash "col" first-m))
        (message "Match 1 of %d at line %d, col %d%s" (length matches)
                 (gethash "line" first-m) (gethash "col" first-m)
                 (thither-large--limit-note))))))

(defun thither-large-search-more ()
  "Fetch the matches after those of the last search that stopped at its limit."
  (interactive)
  (unless (and thither-large--search-matches (> (length thither-large--search-matches) 0))
    (user-error "No active search results"))
  (unless thither-large--search-more
    (user-error "All matches are shown"))
  (let* ((last (aref thither-large--search-matches (1- (length thither-large--search-matches))))
         (more (thither-large--search-batch (+ (gethash "off" last) (gethash "len" last)))))
    (setq thither-large--search-matches (vconcat thither-large--search-matches more))
    (thither-large--search-show)
    (message "%d more matches, %d in all%s" (length more)
             (length thither-large--search-matches) (thither-large--limit-note))))

(defun thither-large-search-next ()
  "Jump to the next search match."
  (interactive)
  (unless (and thither-large--search-matches (> (length thither-large--search-matches) 0))
    (user-error "No active search results"))
  (setq thither-large--search-index
        (mod (1+ thither-large--search-index) (length thither-large--search-matches)))
  (let* ((m (aref thither-large--search-matches thither-large--search-index))
         (line (gethash "line" m))
         (col (gethash "col" m)))
    (thither-large-goto-line line col)
    (message "Match %d of %d at line %d, col %d%s"
             (1+ thither-large--search-index)
             (length thither-large--search-matches)
             line col (thither-large--limit-note))))

(defun thither-large-search-prev ()
  "Jump to the previous search match."
  (interactive)
  (unless (and thither-large--search-matches (> (length thither-large--search-matches) 0))
    (user-error "No active search results"))
  (setq thither-large--search-index
        (mod (1- (+ thither-large--search-index (length thither-large--search-matches)))
             (length thither-large--search-matches)))
  (let* ((m (aref thither-large--search-matches thither-large--search-index))
         (line (gethash "line" m))
         (col (gethash "col" m)))
    (thither-large-goto-line line col)
    (message "Match %d of %d at line %d, col %d%s"
             (1+ thither-large--search-index)
             (length thither-large--search-matches)
             line col (thither-large--limit-note))))

;;;; Keymap and Major Mode

(defmacro thither-large--def-cmd-or-char (name cmd char)
  "Define NAME: executes CMD when buffer is read-only, otherwise self-inserts CHAR."
  `(defun ,name ()
     (interactive)
     (if buffer-read-only
         (call-interactively #',cmd)
       (self-insert-command 1 ,char))))

(thither-large--def-cmd-or-char thither-large-page-next-or-n thither-large-page-next ?n)
(thither-large--def-cmd-or-char thither-large-page-prev-or-p thither-large-page-prev ?p)
(thither-large--def-cmd-or-char thither-large-goto-line-or-g thither-large-goto-line ?g)
(thither-large--def-cmd-or-char thither-large-goto-percent-or-pct thither-large-goto-percent ?%)
(thither-large--def-cmd-or-char thither-large-search-or-s thither-large-search ?s)
(thither-large--def-cmd-or-char thither-large-head-or-lt thither-large-head ?<)
(thither-large--def-cmd-or-char thither-large-tail-or-gt thither-large-tail ?>)

(defvar thither-large-file-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Commands always available
    (define-key map (kbd "C-c C-n") #'thither-large-page-next)
    (define-key map (kbd "C-c C-p") #'thither-large-page-prev)
    (define-key map (kbd "C-c C-g") #'thither-large-goto-line)
    (define-key map (kbd "C-c C-%") #'thither-large-goto-percent)
    (define-key map (kbd "C-c C-s") #'thither-large-search)
    (define-key map (kbd "C-c <")   #'thither-large-head)
    (define-key map (kbd "C-c >")   #'thither-large-tail)
    (define-key map (kbd "M-n")     #'thither-large-search-next)
    (define-key map (kbd "M-p")     #'thither-large-search-prev)
    (define-key map [prior]         #'thither-large-page-prev)
    (define-key map [next]          #'thither-large-page-next)

    ;; Single-character commands active in read-only / view mode
    (define-key map (kbd "n") #'thither-large-page-next-or-n)
    (define-key map (kbd "p") #'thither-large-page-prev-or-p)
    (define-key map (kbd "g") #'thither-large-goto-line-or-g)
    (define-key map (kbd "%") #'thither-large-goto-percent-or-pct)
    (define-key map (kbd "s") #'thither-large-search-or-s)
    (define-key map (kbd "<") #'thither-large-head-or-lt)
    (define-key map (kbd ">") #'thither-large-tail-or-gt)
    map)
  "Keymap for `thither-large-file-mode'.")

(define-derived-mode thither-large-file-mode fundamental-mode "Thither-Large"
  "Major mode for sliding-window viewport editing of remote large files.
\\{thither-large-file-mode-map}"
  :group 'thither-large
  (add-hook 'write-contents-functions #'thither-large-save-buffer nil t)
  (setq header-line-format '(:eval (thither-large-header-string)))
  (force-mode-line-update))

;; `set-visited-file-name' (`write-file', save-as) keeps a special mode:
;; the buffer holds a window, not the file, whatever its new name.
(put 'thither-large-file-mode 'mode-class 'special)

(defun thither-large-setup-buffer (filename host localpath &optional stat window-lines chunk-size)
  "Initialize current buffer for large file viewing/editing.
FILENAME is the full thither file name, HOST is the connection host,
LOCALPATH is the absolute remote path.  STAT is optional stat hash table."
  (let* ((conn (thither-connection host))
         (cs (max 4096 (or chunk-size thither-large-chunk-size 65536)))
         (idx (thither-lineindex conn localpath cs)))
    (setq-local thither-large-host host)
    (setq-local thither-large-path localpath)
    (setq-local thither-large-chunk-size cs)
    (setq-local thither-large-window-lines (or window-lines thither-large-window-lines 5000))
    (setq-local buffer-file-name (thither--expand-name filename))
    (setq-local thither-large-file-name buffer-file-name)
    (thither-large--set-index idx)
    (thither--set-etag buffer-file-name thither-large-etag)
    (thither-large-file-mode)
    (let ((writable (if stat
                        (/= (logand (or (gethash "mode" stat) 0) #o200) 0)
                      t)))
      (setq buffer-read-only (not writable)
            thither-large--forced-read-only nil))
    (let ((end-line (min (max 1 thither-large-total-lines) thither-large-window-lines)))
      (thither-large-load-window 1 end-line))
    (set-buffer-modified-p nil)))

(defun thither-large--after-find-file ()
  "Hook in `find-file-hook' to re-apply mode after `normal-mode'."
  (when (bound-and-true-p thither-large--pending-init)
    (setq thither-large--pending-init nil)
    (thither-large-file-mode)
    ;; `after-find-file' set `buffer-read-only' from the file's permissions
    (setq thither-large--forced-read-only nil)
    (thither-large--update-read-only)))

(add-hook 'find-file-hook #'thither-large--after-find-file)

;;;; Interactive Entry Point

;;;###autoload
(defun thither-find-large-file (filename)
  "Visit remote FILENAME directly in `thither-large-file-mode'."
  (interactive
   (list (read-file-name "Find large remote file: " "/thither:")))
  (let* ((p (thither--path filename))
         (host (car p))
         (localpath (cdr p))
         (st (thither-stat (thither-connection host) localpath))
         (buf (generate-new-buffer (file-name-nondirectory filename))))
    (switch-to-buffer buf)
    (thither-large-setup-buffer filename host localpath st)
    buf))

(provide 'thither-large)
;;; thither-large.el ends here
