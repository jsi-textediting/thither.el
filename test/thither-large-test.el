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

(provide 'thither-large-test)
;;; thither-large-test.el ends here
