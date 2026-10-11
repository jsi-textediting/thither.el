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

;;;; Buffer-local variables

(defvar-local thither-large-host nil
  "Host of the current large remote file.")
(defvar-local thither-large-path nil
  "Host path of the current large remote file.")
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
(defvar-local thither-large-coding-system 'utf-8
  "Coding system for decoding and encoding the viewport text.")
(defvar-local thither-large--search-matches nil
  "Vector of recent search match hash tables.")
(defvar-local thither-large--search-index 0
  "Index of currently selected search match.")
(defvar-local thither-large--search-pattern nil
  "Pattern from the most recent remote search.")
(defvar-local thither-large--pending-init nil
  "Internal flag indicating `thither-large-file-mode' should be established.")

;; Preserve state across major-mode resets (e.g. from `normal-mode')
(dolist (v '(thither-large-host
             thither-large-path
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
             thither-large-coding-system
             thither-large--search-matches
             thither-large--search-index
             thither-large--search-pattern
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
           (conn (thither-connection thither-large-host))
           (data (thither-read-range conn thither-large-path byte-start chunk-len thither-large-etag)))
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

(defun thither-large--line-range-to-byte-range (start-line end-line)
  "Resolve byte range (START-BYTE . END-BYTE) for lines [START-LINE, END-LINE]."
  (let ((start-byte (thither-large--line-to-byte-offset start-line))
        (end-byte (if (>= end-line thither-large-total-lines)
                      thither-large-size
                    (thither-large--line-to-byte-offset (1+ end-line)))))
    (cons start-byte end-byte)))

;;;; Viewport Window Management

(defun thither-large-header-string ()
  "Generate status string for the viewport header line."
  (let ((pct (if (> thither-large-total-lines 0)
                 (* 100.0 (/ (float thither-large-window-start-line)
                             (float thither-large-total-lines)))
               0.0)))
    (format " Thither Large: %s | %s | Lines %s-%s of %s (%.1f%%) | [n/p] page, [g] goto, [s] search "
            (file-name-nondirectory (or buffer-file-name thither-large-path ""))
            (file-size-human-readable thither-large-size)
            thither-large-window-start-line
            thither-large-window-end-line
            thither-large-total-lines
            pct)))

(defun thither-large-load-window (start-line end-line)
  "Load lines [START-LINE, END-LINE] into the buffer."
  (if (<= thither-large-total-lines 0)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq thither-large-window-start-line 1
              thither-large-window-end-line 1
              thither-large-window-start-byte 0
              thither-large-window-end-byte 0
              thither-large-window-orig-text "")
        (set-buffer-modified-p nil))
    (setq start-line (max 1 (min start-line thither-large-total-lines)))
    (setq end-line (max start-line (min end-line thither-large-total-lines)))
    (let* ((range (thither-large--line-range-to-byte-range start-line end-line))
           (start-byte (car range))
           (end-byte (cdr range))
           (len (- end-byte start-byte))
           (conn (thither-connection thither-large-host))
           (raw-bytes (if (> len 0)
                          (thither-read-range conn thither-large-path start-byte len thither-large-etag)
                        ""))
           (text (decode-coding-string raw-bytes (or thither-large-coding-system 'utf-8)))
           (inhibit-read-only t))
      (erase-buffer)
      (insert text)
      (setq thither-large-window-start-line start-line
            thither-large-window-end-line end-line
            thither-large-window-start-byte start-byte
            thither-large-window-end-byte end-byte
            thither-large-window-orig-text raw-bytes)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (force-mode-line-update))))

;;;; Differential Saving

(defun thither-large-save-buffer ()
  "Save the current viewport back to the remote file via `thither-apply-edit'."
  (interactive)
  (unless (buffer-modified-p)
    (message "(No changes need to be saved)")
    t)
  (when (buffer-modified-p)
    (let* ((coding (or coding-system-for-write buffer-file-coding-system 'utf-8))
           (encoded (encode-coding-string (buffer-string) coding t)))
      (if (string= encoded thither-large-window-orig-text)
          (progn
            (set-buffer-modified-p nil)
            (message "(No changes need to be saved)")
            t)
        (let* ((conn (thither-connection thither-large-host))
               (script (vector))
               (inserts (vector encoded)))
          ;; Kept range before window
          (when (> thither-large-window-start-byte 0)
            (setq script (vconcat script
                                  (list `(("keep" . t)
                                          ("off" . 0)
                                          ("len" . ,thither-large-window-start-byte))))))
          ;; Inserted window content (1-based index)
          (setq script (vconcat script (list `(("ins" . 1)))))
          ;; Kept range after window
          (when (< thither-large-window-end-byte thither-large-size)
            (setq script (vconcat script
                                  (list `(("keep" . t)
                                          ("off" . ,thither-large-window-end-byte)
                                          ("len" . ,(- thither-large-size thither-large-window-end-byte)))))))
          (condition-case err
              (let ((res (thither-apply-edit conn thither-large-path thither-large-etag
                                             script inserts
                                             :chunk-size thither-large-chunk-size)))
                (setq thither-large-etag (gethash "etag" res)
                      thither-large-size (gethash "size" res)
                      thither-large-chunks (gethash "chunks" res)
                      thither-large-ends-with-nl (gethash "ends_with_nl" res)
                      thither-large-window-orig-text encoded
                      thither-large-window-end-byte (+ thither-large-window-start-byte (length encoded)))
                (thither--set-etag (or buffer-file-name thither-large-path) thither-large-etag)
                (thither-large--compute-prefix-sums)
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
            (thither-error
             (let ((code (cadr err)))
               (if (equal code "conflict")
                   (error "Conflict! Remote file changed on host (etag: %s). Save aborted"
                          (gethash "etag" (cadddr err)))
                 (signal (car err) (cdr err)))))))))))

;;;; Navigation Commands

(defun thither-large-goto-line (line &optional col)
  "Move the large-file viewport so LINE is visible, and place point there.
COL is optional 1-based column number."
  (interactive "nGo to line: ")
  (when (buffer-modified-p)
    (if (y-or-n-p "Buffer has unsaved changes. Save before moving viewport? ")
        (thither-large-save-buffer)
      (user-error "Aborted viewport move due to unsaved changes")))
  (setq line (max 1 (min line (max 1 thither-large-total-lines))))
  (let* ((win-size (or thither-large-window-lines 5000))
         (half (/ win-size 2))
         (new-start (max 1 (- line half)))
         (new-end (min thither-large-total-lines (+ new-start win-size -1))))
    (when (= new-end thither-large-total-lines)
      (setq new-start (max 1 (- new-end win-size -1))))
    (thither-large-load-window new-start new-end)
    (let ((buf-line (1+ (- line thither-large-window-start-line))))
      (goto-char (point-min))
      (forward-line (1- buf-line))
      (when col
        (move-to-column (max 0 (1- col)))))))

(defun thither-large-goto-percent (pct)
  "Move the viewport to PCT (0-100) percent of the file."
  (interactive "nGo to percent (0-100): ")
  (if (<= thither-large-total-lines 0)
      (thither-large-goto-line 1)
    (let ((target (max 1 (floor (* thither-large-total-lines (/ (float pct) 100.0))))))
      (thither-large-goto-line target))))

(defun thither-large-page-next ()
  "Scroll the viewport forward by ~75% of window size."
  (interactive)
  (let* ((step (max 1 (floor (* (or thither-large-window-lines 5000) 0.75))))
         (target (+ thither-large-window-start-line step)))
    (thither-large-goto-line (min thither-large-total-lines target))))

(defun thither-large-page-prev ()
  "Scroll the viewport backward by ~75% of window size."
  (interactive)
  (let* ((step (max 1 (floor (* (or thither-large-window-lines 5000) 0.75))))
         (target (- thither-large-window-start-line step)))
    (thither-large-goto-line (max 1 target))))

(defun thither-large-head ()
  "Jump viewport to the beginning of the file."
  (interactive)
  (thither-large-goto-line 1))

(defun thither-large-tail ()
  "Jump viewport to the end of the file."
  (interactive)
  (thither-large-goto-line (max 1 thither-large-total-lines)))

;;;; Search Integration

(defvar thither-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'thither-search-goto-match)
    (define-key map [mouse-2] #'thither-search-goto-match)
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

(defun thither-large-search (pattern &optional regex case)
  "Search the entire remote file for PATTERN using `thither-search'.
With prefix argument, prompt for REGEX and CASE sensitivity."
  (interactive
   (let* ((def (or (and (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                   (thing-at-point 'symbol)
                   ""))
          (prompt (if (string-empty-p def) "Search pattern: " (format "Search pattern (default %s): " def)))
          (pat (read-string prompt nil nil def)))
     (if current-prefix-arg
         (list pat (y-or-n-p "Regular expression? ") (y-or-n-p "Case-sensitive? "))
       (list pat nil nil))))
  (let* ((conn (thither-connection thither-large-host))
         (matches (thither-search conn thither-large-path pattern
                                  :regex regex :case case
                                  :etag thither-large-etag)))
    (if (or (null matches) (= (length matches) 0))
        (message "No matches found for: %s" pattern)
      (setq thither-large--search-matches matches
            thither-large--search-index 0
            thither-large--search-pattern pattern)
      (let ((search-buf (get-buffer-create "*thither-search*"))
            (src-buf (current-buffer))
            (count (length matches)))
        (with-current-buffer search-buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "%d matches for \"%s\" in %s:\n\n"
                            count pattern (buffer-name src-buf)))
            (dotimes (i count)
              (let* ((m (aref matches i))
                     (line (gethash "line" m))
                     (col (gethash "col" m))
                     (off (gethash "off" m))
                     (len (gethash "len" m))
                     (start-pos (point)))
                (insert (format "Line %d, Col %d (byte %d, len %d)\n" line col off len))
                (add-text-properties start-pos (point)
                                     (list 'thither-search-match m
                                           'thither-source-buffer src-buf
                                           'mouse-face 'highlight
                                           'help-echo "mouse-2, RET: jump to match"))))
            (goto-char (point-min))
            (forward-line 2)
            (thither-search-mode)))
        (display-buffer search-buf)
        (let ((first-m (aref matches 0)))
          (thither-large-goto-line (gethash "line" first-m) (gethash "col" first-m))
          (message "Match 1 of %d at line %d, col %d" count
                   (gethash "line" first-m) (gethash "col" first-m)))))))

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
    (message "Match %d of %d at line %d, col %d"
             (1+ thither-large--search-index)
             (length thither-large--search-matches)
             line col)))

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
    (message "Match %d of %d at line %d, col %d"
             (1+ thither-large--search-index)
             (length thither-large--search-matches)
             line col)))

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

(defun thither-large-setup-buffer (filename host localpath &optional stat window-lines chunk-size)
  "Initialize current buffer for large file viewing/editing.
FILENAME is the full thither file name, HOST is the connection host,
LOCALPATH is the absolute remote path.  STAT is optional stat hash table."
  (let* ((conn (thither-connection host))
         (cs (max 4096 (or chunk-size thither-large-chunk-size 65536)))
         (idx (thither-lineindex conn localpath cs)))
    (setq-local thither-large-host host)
    (setq-local thither-large-path localpath)
    (setq-local thither-large-etag (gethash "etag" idx))
    (setq-local thither-large-size (gethash "size" idx))
    (setq-local thither-large-chunks (gethash "chunks" idx))
    (setq-local thither-large-ends-with-nl (gethash "ends_with_nl" idx))
    (setq-local thither-large-chunk-size cs)
    (setq-local thither-large-window-lines (or window-lines thither-large-window-lines 5000))
    (setq-local buffer-file-name (thither--expand-name filename))
    (thither--set-etag buffer-file-name thither-large-etag)
    (thither-large--compute-prefix-sums)
    (let ((end-line (min (max 1 thither-large-total-lines) thither-large-window-lines)))
      (thither-large-load-window 1 end-line))
    (thither-large-file-mode)
    (let ((writable (if stat
                        (/= (logand (or (gethash "mode" stat) 0) #o200) 0)
                      t)))
      (setq buffer-read-only (not writable)))
    (set-buffer-modified-p nil)))

(defun thither-large--after-find-file ()
  "Hook in `find-file-hook' to re-apply mode after `normal-mode'."
  (when (bound-and-true-p thither-large--pending-init)
    (setq thither-large--pending-init nil)
    (thither-large-file-mode)))

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
