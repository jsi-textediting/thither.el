;;; thither-large-test.el --- ERT tests for thither-large.el  -*- lexical-binding: t; no-byte-compile: t; -*-

;; Run with:
;;   THITHER_SERVER_BIN="$PWD/thither/build/thither-server" sh ./thither.el/test/run.sh local test/thither-large-test.el

(require 'ert)
(require 'thither)
(require 'thither-fs)
(require 'thither-proc)
(require 'thither-large)

(defun thither-large-test--command ()
  (let ((s (getenv "THITHER_SERVER"))) (and s (split-string-and-unquote s))))

(setq thither-command-function (lambda (_host) (thither-large-test--command)))
(thither-fs-enable)

(defvar thither-large-test--host "t")

(defun thither-large-test--sh (script)
  (thither--text (plist-get (thither-exec (thither-connection thither-large-test--host) (list "sh" "-c" script))
                            :stdout)))

(defmacro thither-large-test--with-dir (var &rest body)
  "Run BODY with VAR bound to a fresh remote directory name."
  (declare (indent 1))
  `(progn
     (unless (thither-large-test--command) (ert-skip "THITHER_SERVER not set"))
     (let* ((local (string-trim (thither-large-test--sh "mktemp -d")))
            (,var (format "/thither:%s:%s/" thither-large-test--host local)))
       (unwind-protect (progn ,@body)
         (ignore-errors (thither-large-test--sh (format "rm -rf %s" local)))))))

;;;; Protocol Operations

(ert-deftest thither-large-test-lineindex ()
  (thither-large-test--with-dir dir
    (let* ((file (concat dir "sample.txt"))
           (p (thither--path file))
           (conn (thither-connection (car p))))
      (write-region "line 1\nline 2\nline 3\n" nil file nil 'silent)
      (let ((idx (thither-lineindex conn (cdr p) 4096)))
        (should (gethash "etag" idx))
        (should (= 21 (gethash "size" idx)))
        (should (equal t (gethash "ends_with_nl" idx)))
        (should (> (length (gethash "chunks" idx)) 0))))))

(ert-deftest thither-large-test-read-range ()
  (thither-large-test--with-dir dir
    (let* ((file (concat dir "range.txt"))
           (p (thither--path file))
           (conn (thither-connection (car p))))
      (write-region "0123456789abcdef" nil file nil 'silent)
      (let ((slice (thither-read-range conn (cdr p) 5 6)))
        (should (equal "56789a" slice))))))

;;;; Mathematics & Prefix Sums

(ert-deftest thither-large-test-prefix-sums-and-offsets ()
  (thither-large-test--with-dir dir
    (let* ((file (concat dir "multi.txt"))
           (p (thither--path file))
           (host (car p))
           (localpath (cdr p))
           (content "alpha\nbeta\ngamma\ndelta\nepsilon\n"))
      (write-region content nil file nil 'silent)
      (with-temp-buffer
        (thither-large-setup-buffer file host localpath nil 2 4096)
        (should (= 5 thither-large-total-lines))
        (should (equal t thither-large-ends-with-nl))
        ;; Line 1 starts at byte 0
        (should (= 0 (thither-large--line-to-byte-offset 1)))
        ;; Line 2 starts immediately after "alpha\n" (len 6) -> byte 6
        (should (= 6 (thither-large--line-to-byte-offset 2)))
        ;; Line 3 starts after "alpha\nbeta\n" (len 6+5=11) -> byte 11
        (should (= 11 (thither-large--line-to-byte-offset 3)))
        ;; Line range [2, 3] covers "beta\ngamma\n" -> bytes [6, 17)
        (let ((r (thither-large--line-range-to-byte-range 2 3)))
          (should (= 6 (car r)))
          (should (= 17 (cdr r))))))))

;;;; Viewport Loading & Navigation

(ert-deftest thither-large-test-viewport-window ()
  (thither-large-test--with-dir dir
    (let* ((file (concat dir "view.txt"))
           (p (thither--path file))
           (host (car p))
           (localpath (cdr p)))
      ;; 10 lines
      (write-region "1:one\n2:two\n3:three\n4:four\n5:five\n6:six\n7:seven\n8:eight\n9:nine\n10:ten\n"
                    nil file nil 'silent)
      (with-temp-buffer
        ;; Set window size to 3 lines
        (thither-large-setup-buffer file host localpath nil 3 4096)
        (should (eq major-mode 'thither-large-file-mode))
        (should (= 10 thither-large-total-lines))
        (should (= 1 thither-large-window-start-line))
        (should (= 3 thither-large-window-end-line))
        (should (equal "1:one\n2:two\n3:three\n" (buffer-string)))

        ;; Navigate to line 5
        (thither-large-goto-line 5)
        (should (>= 5 thither-large-window-start-line))
        (should (<= 5 thither-large-window-end-line))
        (should (string-match-p "5:five" (buffer-string)))

        ;; Page next
        (thither-large-page-next)
        (should (> thither-large-window-start-line 1))

        ;; Head and tail
        (thither-large-tail)
        (should (= 10 thither-large-window-end-line))
        (thither-large-head)
        (should (= 1 thither-large-window-start-line))))))

;;;; Differential Saving via apply_edit

(ert-deftest thither-large-test-in-window-save ()
  (thither-large-test--with-dir dir
    (let* ((file (concat dir "edit.txt"))
           (p (thither--path file))
           (host (car p))
           (localpath (cdr p)))
      (write-region "first\nsecond\nthird\nfourth\nfifth\n" nil file nil 'silent)
      (with-temp-buffer
        (thither-large-setup-buffer file host localpath nil 3 4096)
        ;; Window is lines 1-3: "first\nsecond\nthird\n"
        (should (equal "first\nsecond\nthird\n" (buffer-string)))

        ;; Modify line 2: change "second" to "SECOND_MODIFIED"
        (goto-char (point-min))
        (search-forward "second")
        (replace-match "SECOND_MODIFIED")
        (should (buffer-modified-p))

        ;; Save via thither-large-save-buffer
        (should (thither-large-save-buffer))
        (should-not (buffer-modified-p))

        ;; Verify remote file on host has updated line 2 and intact lines 4-5
        (let* ((conn (thither-connection host))
               (full (thither-read-file conn localpath)))
          (should (equal "first\nSECOND_MODIFIED\nthird\nfourth\nfifth\n" (car full))))))))

;;;; Conflict Detection

(ert-deftest thither-large-test-conflict-handling ()
  (thither-large-test--with-dir dir
    (let* ((file (concat dir "conflict.txt"))
           (p (thither--path file))
           (host (car p))
           (localpath (cdr p)))
      (write-region "line A\nline B\nline C\n" nil file nil 'silent)
      (with-temp-buffer
        (thither-large-setup-buffer file host localpath nil 3 4096)
        ;; Change in buffer
        (goto-char (point-min))
        (insert "EDIT-")

        ;; Modify out-of-band on remote host (bumps etag)
        (let ((conn (thither-connection host)))
          (thither-write-file conn localpath "EXTERNAL OVERWRITE\n"))

        ;; Saving should detect conflict and error out
        (should-error (thither-large-save-buffer))))))

;;;; Server-Side Search

(ert-deftest thither-large-test-server-search ()
  (thither-large-test--with-dir dir
    (let* ((file (concat dir "search.txt"))
           (p (thither--path file))
           (host (car p))
           (localpath (cdr p)))
      (write-region "apple pie\nbanana split\ncherry pie\ndate shake\npecan pie\n"
                    nil file nil 'silent)
      (with-temp-buffer
        (thither-large-setup-buffer file host localpath nil 2 4096)
        (thither-large-search "pie")
        (should (= 3 (length thither-large--search-matches)))
        ;; Jump to next match
        (thither-large-search-next)
        (should (string-match-p "cherry pie" (buffer-string)))
        ;; Previous match
        (thither-large-search-prev)
        (should (string-match-p "apple pie" (buffer-string)))))))

;;;; Automatic Routing via find-file

(ert-deftest thither-large-test-find-file-threshold ()
  (thither-large-test--with-dir dir
    (let* ((file (concat dir "thresh.txt"))
           (content (mapconcat (lambda (i) (format "entry %d\n" i)) (number-sequence 1 100) "")))
      (write-region content nil file nil 'silent)
      ;; Force low threshold and disable confirmation prompt
      (let ((thither-large-file-threshold 50)
            (thither-large-confirm nil))
        (let ((buf (find-file-noselect file)))
          (unwind-protect
              (with-current-buffer buf
                (should (eq major-mode 'thither-large-file-mode))
                (should (= 100 thither-large-total-lines)))
            (kill-buffer buf)))))))

(ert-deftest thither-large-test-big-insert-and-save-as ()
  ;; an insert over the frame limit goes up as a blob; save-as uses `dest'
  (thither-large-test--with-dir dir
    (let* ((file (concat dir "big.txt"))
           (copy (concat dir "copy.txt"))
           (p (thither--path file))
           (big (make-string (* 9 1024 1024) ?z)))
      (write-region "a\r\nb\r\n" nil file nil 'silent)
      (with-temp-buffer
        (thither-large-setup-buffer file (car p) (cdr p) nil 10 4096)
        (goto-char (point-min))
        (insert big)
        (should (thither-large-save-buffer))
        (should (= (+ (length big) 6) thither-large-size))
        (let ((head (thither-read-range (thither-connection (car p)) (cdr p)
                                        (length big) 6)))
          (should (equal "a\r\nb\r\n" head)))
        (setq buffer-file-name copy)
        (set-buffer-modified-p t)
        (should (thither-large-save-buffer))
        (should (equal (cdr (thither--path copy)) thither-large-path))
        (should (= (+ (length big) 6)
                   (gethash "size" (thither-stat (thither-connection (car p))
                                                 (cdr (thither--path copy))))))))))

;;;; Offline tests: a fake host in memory, no server needed

(defvar thither-large-test--files nil
  "Hash table: host path -> unibyte content of the fake host.")
(defvar thither-large-test--caps '("apply_edit_dest")
  "Capabilities of the fake host.")
(defvar thither-large-test--edits nil
  "Arguments of the `thither-apply-edit' calls, latest first.")

(defun thither-large-test--etag (data) (secure-hash 'md5 data))

(defun thither-large-test--index (data &optional chunk-size)
  (let ((cs (or chunk-size 4096)) (chunks nil) (pos 0) (n (length data))
        (h (make-hash-table :test 'equal)))
    (while (< pos n)
      (let ((end (min n (+ pos cs))))
        (push (vector (- end pos) (cl-count ?\n (substring data pos end))) chunks)
        (setq pos end)))
    (puthash "etag" (thither-large-test--etag data) h)
    (puthash "size" n h)
    (puthash "chunks" (vconcat (nreverse chunks)) h)
    ;; as the msgpack decoder delivers it
    (puthash "ends_with_nl" (if (and (> n 0) (= (aref data (1- n)) ?\n)) t :false) h)
    h))

(defun thither-large-test--file (path)
  (or (gethash path thither-large-test--files)
      (signal 'thither-error (list "ENOENT" "no such file" nil))))

(defun thither-large-test--check-etag (path etag code)
  (unless (equal etag (thither-large-test--etag (thither-large-test--file path)))
    (let ((e (make-hash-table :test 'equal)))
      (puthash "etag" (thither-large-test--etag (thither-large-test--file path)) e)
      (signal 'thither-error (list code "etag mismatch" e)))))

(defun thither-large-test--apply-edit (_conn path etag script inserts &rest keys)
  (push (list path etag script inserts keys) thither-large-test--edits)
  (thither-large-test--check-etag path etag "conflict")
  (let* ((old (thither-large-test--file path))
         (new (mapconcat (lambda (item)
                           (if (alist-get "ins" item nil nil #'equal)
                               (aref inserts (1- (alist-get "ins" item nil nil #'equal)))
                             (let ((off (alist-get "off" item nil nil #'equal)))
                               (substring old off (+ off (alist-get "len" item nil nil #'equal))))))
                         script ""))
         (dest (plist-get keys :dest)))
    (puthash (or dest path) new thither-large-test--files)
    (thither-large-test--index new)))

(defun thither-large-test--search (_conn path pattern &rest keys)
  (let* ((data (thither-large-test--file path))
         (case-fold-search (not (plist-get keys :case)))
         (limit (or (plist-get keys :limit) 1000))
         (pos (or (plist-get keys :from-off) 0))
         (out nil))
    (while (and (< (length out) limit) (string-match (regexp-quote pattern) data pos))
      (let* ((off (match-beginning 0))
             (bol (let ((p off)) (while (and (> p 0) (/= (aref data (1- p)) ?\n)) (cl-decf p)) p))
             (m (make-hash-table :test 'equal)))
        (puthash "off" off m)
        (puthash "len" (- (match-end 0) off) m)
        (puthash "line" (1+ (cl-count ?\n (substring data 0 off))) m)
        (puthash "col" (1+ (- off bol)) m)
        (push m out)
        (setq pos (match-end 0))))
    (vconcat (nreverse out))))

(defmacro thither-large-test--offline (files lines &rest body)
  "Run BODY in a buffer visiting the first of FILES on a fake host.
FILES is a list of (PATH . CONTENT); the window has LINES lines."
  (declare (indent 2))
  `(let ((thither-large-test--files (make-hash-table :test 'equal))
         (thither-large-test--edits nil)
         (create-lockfiles nil))
     (dolist (f ,files)
       (puthash (car f) (if (multibyte-string-p (cdr f)) (encode-coding-string (cdr f) 'utf-8 t) (cdr f))
                thither-large-test--files))
     (cl-letf (((symbol-function 'thither-connection) (lambda (_host) 'fake))
               ((symbol-function 'thither--expand-name) #'identity)
               ((symbol-function 'thither-has-cap)
                (lambda (_conn cap) (member cap thither-large-test--caps)))
               ((symbol-function 'thither-lineindex)
                (lambda (_conn path &optional cs _timeout)
                  (thither-large-test--index (thither-large-test--file path) cs)))
               ((symbol-function 'thither-read-range)
                (lambda (_conn path off len &optional etag _timeout)
                  (when etag (thither-large-test--check-etag path etag "stale"))
                  (let ((data (thither-large-test--file path)))
                    (substring data (min off (length data)) (min (length data) (+ off len))))))
               ((symbol-function 'thither-stat)
                (lambda (_conn path &optional _nofollow)
                  (let ((data (gethash path thither-large-test--files)))
                    (when data
                      (let ((h (make-hash-table :test 'equal)))
                        (puthash "etag" (thither-large-test--etag data) h)
                        (puthash "mode" #o644 h)
                        h)))))
               ((symbol-function 'thither-apply-edit) #'thither-large-test--apply-edit)
               ((symbol-function 'thither-search) #'thither-large-test--search)
               ((symbol-function 'set-visited-file-name)
                (lambda (name &rest _) (setq buffer-file-name name))))
       (with-temp-buffer
         (buffer-enable-undo)
         (unwind-protect
             (progn
               (thither-large-setup-buffer (concat "/thither:fake:" (car (car ,files))) "fake"
                                           (car (car ,files)) nil ,lines 4096)
               ,@body)
           (set-buffer-modified-p nil))))))

(defun thither-large-test--host-file (path)
  (gethash path thither-large-test--files))

(defun thither-large-test--numbered (n)
  (mapconcat (lambda (i) (format "%d:line\n" i)) (number-sequence 1 n) ""))

(ert-deftest thither-large-test-offline-no-final-newline ()
  (thither-large-test--offline '(("/f" . "one\ntwo\nthree")) 2
    (should-not thither-large-ends-with-nl)
    (should (= 3 thither-large-total-lines))
    (thither-large-tail)
    (should (= 3 thither-large-window-end-line))
    (should (equal "two\nthree" (buffer-string)))
    (should (looking-at "three"))))

(ert-deftest thither-large-test-offline-no-newline-at-all ()
  ;; a minified file: one line, no newline
  (thither-large-test--offline '(("/f" . "abcdef")) 10
    (should (= 1 thither-large-total-lines))
    (should (equal "abcdef" (buffer-string)))
    (goto-char (point-max))
    (insert "X")
    (should (thither-large-save-buffer))
    (should (equal "abcdefX" (thither-large-test--host-file "/f")))))

(ert-deftest thither-large-test-offline-crlf ()
  (thither-large-test--offline '(("/f" . "a\r\nb\r\nc\r\n")) 10
    (should (equal "a\nb\nc\n" (buffer-string)))
    (should (eq 1 (coding-system-eol-type thither-large-window-coding)))
    (goto-char (point-min))
    (search-forward "b")
    (replace-match "B")
    (should (thither-large-save-buffer))
    (should (equal "a\r\nB\r\nc\r\n" (thither-large-test--host-file "/f")))))

(ert-deftest thither-large-test-offline-mixed-eol ()
  (thither-large-test--offline '(("/f" . "a\r\nb\nc\r\n")) 10
    (goto-char (point-min))
    (search-forward "b")
    (replace-match "B")
    (should (thither-large-save-buffer))
    (should (equal "a\r\nB\nc\r\n" (thither-large-test--host-file "/f")))))

(ert-deftest thither-large-test-offline-default-coding-ignored ()
  ;; a DOS default (as on Windows) must not add CRs to an LF file
  (let ((old (default-value 'buffer-file-coding-system)))
    (unwind-protect
        (progn
          (setq-default buffer-file-coding-system 'utf-8-dos)
          (thither-large-test--offline '(("/f" . "x\ny\nh\303\251\n")) 10
            (should (equal "x\ny\nhé\n" (buffer-string)))
            (goto-char (point-min))
            (search-forward "y")
            (replace-match "Y")
            (should (thither-large-save-buffer))
            (should (equal "x\nY\nh\303\251\n" (thither-large-test--host-file "/f")))))
      (setq-default buffer-file-coding-system old))))

(ert-deftest thither-large-test-offline-undo-after-window-load ()
  (thither-large-test--offline `(("/f" . ,(thither-large-test--numbered 10))) 3
    (should (null buffer-undo-list))
    (thither-large-page-next)
    (should (null buffer-undo-list))
    (let ((window (buffer-string)))
      (goto-char (point-min))
      (insert "X")
      (let ((pending buffer-undo-list))
        (while pending (setq pending (primitive-undo 1 pending))))
      ;; undo goes back to this window, never to the previous one
      (should (equal window (buffer-string))))))

(ert-deftest thither-large-test-read-range-split ()
  (let ((thither-read-range-max 4) (calls nil) (data "0123456789"))
    (cl-letf (((symbol-function 'thither-call-sync)
               (lambda (_conn op args &optional _timeout)
                 (should (equal op "read_range"))
                 (let ((off (alist-get "off" args nil nil #'equal))
                       (len (alist-get "len" args nil nil #'equal)))
                   (push (cons off len) calls)
                   (substring data (min off 10) (min 10 (+ off len)))))))
      ;; stops at the short answer (end of file)
      (should (equal "123456789" (thither-read-range 'conn "/p" 1 20)))
      (should (equal '((1 . 4) (5 . 4) (9 . 4)) (nreverse calls)))
      (setq calls nil)
      (should (equal "01234567" (thither-read-range 'conn "/p" 0 8)))
      (should (equal '((0 . 4) (4 . 4)) (nreverse calls))))))

(ert-deftest thither-large-test-offline-window-byte-cap ()
  (let ((thither-large-window-max-bytes 15))
    (thither-large-test--offline `(("/f" . ,(thither-large-test--numbered 9))) 100
      ;; "1:line\n" is 7 bytes: two lines fit
      (should (= 2 thither-large-window-end-line))
      (should (equal "1:line\n2:line\n" (buffer-string)))
      ;; paging does not skip the lines the cap left out
      (thither-large-page-next)
      (should (= 3 thither-large-window-start-line)))))

(ert-deftest thither-large-test-offline-truncated-line ()
  (let ((thither-large-window-max-bytes 10))
    (thither-large-test--offline `(("/f" . ,(concat (make-string 30 ?x) "\ny\n"))) 100
      (should thither-large-window-truncated)
      (should buffer-read-only)
      (should (equal (make-string 10 ?x) (buffer-string)))
      (let ((inhibit-read-only t)) (insert "z"))
      (should-error (thither-large-save-buffer) :type 'user-error)
      (set-buffer-modified-p nil)
      (thither-large-goto-line 2)
      (should-not thither-large-window-truncated)
      (should-not buffer-read-only)
      (should (equal "y\n" (buffer-string))))))

(ert-deftest thither-large-test-offline-page-symmetric ()
  (thither-large-test--offline `(("/f" . ,(thither-large-test--numbered 20))) 4
    (thither-large-page-next)
    (should (= 4 thither-large-window-start-line))
    (thither-large-page-next)
    (should (= 7 thither-large-window-start-line))
    (thither-large-page-prev)
    (should (= 4 thither-large-window-start-line))
    (thither-large-page-prev)
    (should (= 1 thither-large-window-start-line))))

(ert-deftest thither-large-test-offline-byte-column ()
  (thither-large-test--offline '(("/f" . "first\nhéllo wörld\n")) 10
    ;; "héllo " is 7 bytes, so "wörld" is at byte column 8
    (thither-large-goto-line 2 8)
    (should (looking-at "wörld"))
    (thither-large-goto-line 2 0)
    (should (bolp))))

(ert-deftest thither-large-test-offline-search-limit ()
  (let ((thither-large-search-limit 2))
    (thither-large-test--offline '(("/f" . "pie\nPIE\npie\npie\n")) 10
      (thither-large-search "pie" nil t)
      (should (= 2 (length thither-large--search-matches)))
      (should thither-large--search-more)
      (thither-large-search-more)
      (should (= 3 (length thither-large--search-matches)))
      (should-not thither-large--search-more)
      (should (= 4 (gethash "line" (aref thither-large--search-matches 2)))))))

(ert-deftest thither-large-test-offline-search-limit-exact ()
  ;; as many matches as the limit: nothing more to fetch
  (let ((thither-large-search-limit 2))
    (thither-large-test--offline '(("/f" . "pie\nPIE\npie\n")) 10
      (thither-large-search "pie" nil t)
      (should (= 2 (length thither-large--search-matches)))
      (should-not thither-large--search-more))))

(ert-deftest thither-large-test-search-case-default ()
  (let (opts)
    (cl-letf (((symbol-function 'thither-call-sync)
               (lambda (_conn _op args &optional _timeout)
                 (setq opts (alist-get "opts" args nil nil #'equal))
                 [])))
      (thither-search 'conn "/p" "x")
      (should (eq t (alist-get "case" opts nil nil #'equal)))
      (thither-search 'conn "/p" "x" :case nil)
      (should (eq :false (alist-get "case" opts nil nil #'equal))))))

(ert-deftest thither-large-test-offline-save-as-same-host ()
  (thither-large-test--offline '(("/f" . "a\nb\nc\nd\n")) 2
    (goto-char (point-min))
    (insert "X")
    (setq buffer-file-name "/thither:fake:/copy")
    (should (thither-large-save-buffer))
    (should (equal "Xa\nb\nc\nd\n" (thither-large-test--host-file "/copy")))
    (should (equal "a\nb\nc\nd\n" (thither-large-test--host-file "/f")))
    (should (equal "/copy" thither-large-path))
    (should (equal "-" (plist-get (nth 4 (car thither-large-test--edits)) :dest-if-match)))
    ;; saving an unchanged buffer under another name still writes the file
    (set-buffer-modified-p t)
    (setq buffer-file-name "/thither:fake:/copy2")
    (should (thither-large-save-buffer))
    (should (equal "Xa\nb\nc\nd\n" (thither-large-test--host-file "/copy2")))))

(ert-deftest thither-large-test-offline-save-as-refused ()
  (thither-large-test--offline '(("/f" . "a\nb\n")) 2
    (goto-char (point-min))
    (insert "X")
    (setq buffer-file-name "/thither:other:/g")
    (should-error (thither-large-save-buffer) :type 'user-error)
    (should (equal "/thither:fake:/f" buffer-file-name))
    (setq buffer-file-name "/tmp/local-copy")
    (should-error (thither-large-save-buffer) :type 'user-error)
    (should (equal "/thither:fake:/f" buffer-file-name))
    (let ((thither-large-test--caps nil))
      (setq buffer-file-name "/thither:fake:/copy")
      (should-error (thither-large-save-buffer) :type 'user-error))
    (should (null thither-large-test--edits))
    (should (equal "a\nb\n" (thither-large-test--host-file "/f")))
    (should-not (thither-large-test--host-file "/copy"))))

(ert-deftest thither-large-test-offline-save-hook-survives-mode-change ()
  (thither-large-test--offline '(("/f" . "a\n")) 2
    (text-mode)
    (should-not (memq #'thither-large-save-buffer write-contents-functions))
    (run-hooks 'before-save-hook)
    (should (memq #'thither-large-save-buffer write-contents-functions))))

(ert-deftest thither-large-test-offline-save-timeout ()
  (thither-large-test--offline '(("/f" . "a\nb\n")) 2
    (goto-char (point-min))
    (insert "X")
    (let ((etag thither-large-etag))
      ;; cancelled before anything was written
      (cl-letf (((symbol-function 'thither-apply-edit)
                 (lambda (&rest _) (signal 'thither-error (list "timeout" "apply_edit timed out" nil)))))
        (should (string-match-p "unchanged"
                                (cadr (should-error (thither-large-save-buffer))))))
      ;; the host finished just before the cancel
      (cl-letf (((symbol-function 'thither-apply-edit)
                 (lambda (&rest args)
                   (apply #'thither-large-test--apply-edit args)
                   (signal 'thither-error (list "timeout" "apply_edit timed out" nil)))))
        (should (string-match-p "may have completed"
                                (cadr (should-error (thither-large-save-buffer))))))
      (should (equal etag thither-large-etag))
      (should (buffer-modified-p)))))

(ert-deftest thither-large-test-offline-conflict ()
  (thither-large-test--offline '(("/f" . "a\nb\n")) 2
    (goto-char (point-min))
    (insert "X")
    (puthash "/f" "changed\n" thither-large-test--files)
    (should (string-match-p "Conflict" (cadr (should-error (thither-large-save-buffer)))))
    (should (equal "changed\n" (thither-large-test--host-file "/f")))))

(provide 'thither-large-test)
;;; thither-large-test.el ends here
