;;; thither-setup.el --- Opt-in setup for /thither:HOST:/path files  -*- lexical-binding: t; -*-

;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: files, comm, processes

;;; Commentary:

;; (require 'thither-setup) registers the file name handler, builds the server
;; command line per host (plink on Windows, ssh elsewhere) and adapts the
;; packages that misbehave on remote files:
;;
;;   - `consult-ripgrep' falls back to `consult-grep' and `consult-fd' to
;;     `consult-find' when the host lacks rg / fd;
;;   - lsp-mode and flycheck are not started in thither buffers (they would need a
;;     server connection of their own).
;;
;; Open files with C-x C-f /thither:HOST:/path or M-x `thither-find-file'.

;;; Code:

(require 'thither-fs)
(require 'thither-proc)
(require 'thither-large)

(declare-function consult-grep "consult")
(declare-function consult-find "consult")

(defgroup thither-setup nil "Setup for thither-server remote files." :group 'thither)

(defcustom thither-server-program "thither-server"
  "Server program on the remote host (a path, or a name found in PATH)."
  :type 'string)

(defcustom thither-host-options nil
  "Per host overrides, an alist of (HOST . PLIST).
PLIST keys: :server (program on that host), :server-args (extra arguments
such as (\"--root\" \"/home/me\")), :session (t or a session name of
[A-Za-z0-9._-], at most 32 characters: run through a persistent
`thither-server --session NAME' daemon on the host; t means \"default\"),
:command (a function of HOST returning the complete command list, replacing
everything else, :session included)."
  :type '(alist :key-type string :value-type plist))

(defcustom thither-hosts nil
  "Host names offered by `thither-find-file'."
  :type '(repeat string))

(defcustom thither-disable-remote-tools t
  "Non-nil: do not start lsp-mode and flycheck in thither buffers."
  :type 'boolean)

(defun thither--host-option (host key)
  (plist-get (cdr (assoc host thither-host-options)) key))

(defun thither--posix-quote (arg)
  "ARG quoted for the POSIX shell that runs the remote command.
Plain words (a leading ~ included, so it still expands) are left alone.  Not
`shell-quote-argument': on Windows that quotes for cmd.exe."
  (if (and (not (string-empty-p arg)) (string-match-p "\\`~?[-A-Za-z0-9_./=:,+@%]*\\'" arg))
      arg
    (concat "'" (replace-regexp-in-string "'" "'\\''" arg t t) "'")))

(defconst thither--session-name-regexp "\\`[A-Za-z0-9._-]\\{1,32\\}\\'")

(defun thither--session-name (host)
  "Session name of HOST's :session option, or nil when it has none.
An invalid value is an error."
  (let ((s (thither--host-option host :session)))
    (cond ((null s) nil)
          ((eq s t) "default")
          ((and (stringp s) (string-match-p thither--session-name-regexp s)) s)
          (t (user-error "thither: bad :session for %s: %S (1-32 characters of A-Z a-z 0-9 . _ -)"
                         host s)))))

(defun thither-launch-command (host)
  "Command list that runs the server on HOST over plink or ssh."
  (let ((custom (thither--host-option host :command)))
    (if custom
        (funcall custom host)
      (let ((session (thither--session-name host)))
        (append (if (eq system-type 'windows-nt)
                    (list (or (executable-find "plink") "plink") "-ssh" "-batch" "-T" host)
                  (list "ssh" "-T" host))
                ;; ssh and plink join these with spaces for the remote shell
                (mapcar #'thither--posix-quote
                        (append (list (or (thither--host-option host :server) thither-server-program))
                                (thither--host-option host :server-args)
                                (list "--stdio")
                                (and session (list "--session" session)))))))))

(defvar tramp-ignored-file-name-regexp)

;;;###autoload
(progn
  ;; Registered when the autoloads are loaded (package.el), or by the :init
  ;; forms shown in the README (use-package with :load-path), so that a
  ;; /thither:HOST:/ name loads this file on first use.
  (autoload 'thither-file-name-handler "thither-setup")
  (add-to-list 'file-name-handler-alist
               (cons "\\`/thither:[^:/]+:" #'thither-file-name-handler))
  ;; TRAMP, if loaded before this file, puts its handler in front and would
  ;; reject the "thither" method; ignored names reach the handler above.
  ;; (Loading this file then moves the handler to the front, see
  ;; `thither-fs-enable'.)
  (let ((old (bound-and-true-p tramp-ignored-file-name-regexp)))
    (unless (and old (string-match-p (regexp-quote "\\`/thither:[^:/]+:") old))
      (setq tramp-ignored-file-name-regexp
            (if old (concat "\\(?:" old "\\)\\|\\(?:\\`/thither:[^:/]+:\\)")
              "\\`/thither:[^:/]+:")))))

;;;###autoload
(defun thither-find-file (host)
  "Visit a file on HOST (a PuTTY session or ssh host) with thither-server."
  (interactive
   (list (completing-read "thither host: "
                          (delete-dups (append thither-hosts (hash-table-keys thither--connections))))))
  (find-file (read-file-name "Find file: " (format "/thither:%s:~/" host))))

;;;###autoload
(defun thither-disconnect-all ()
  "Close every server connection (they are reopened on demand)."
  (interactive)
  (dolist (h (hash-table-keys thither--connections)) (thither-disconnect h)))

;;;; Sessions

(defun thither--read-host (prompt)
  "The host of the current buffer's remote file or directory, else ask with PROMPT.
With a prefix argument always ask."
  (or (and (not current-prefix-arg)
           (car (thither--split (or buffer-file-name default-directory))))
      (completing-read prompt
                       (delete-dups (append thither-hosts (hash-table-keys thither--connections)))
                       nil nil nil nil
                       (car (hash-table-keys thither--connections)))))

(defun thither--session-of (host)
  "Connection to HOST and its hello `session' table; an error without one."
  (let* ((c (thither-connection host))
         (s (thither-get (thither-conn-hello c) "session")))
    (unless (hash-table-p s)
      (user-error "thither: %s is not in session mode (set :session in `thither-host-options')"
                  host))
    (cons c s)))

;;;###autoload
(defun thither-session-info (host)
  "Show the state of HOST's thither-server session (see :session).
Returns the server's `session_info' table."
  (interactive (list (thither--read-host "Session info for thither host: ")))
  (let* ((c (car (thither--session-of host)))
         (i (thither-call-sync c "session_info"))
         (plugins (append (thither-get i "plugins") nil))
         (started (thither-get i "started")))
    (message "thither session %s on %s: pid %s, started %s, version %s, skew %s, %s connection%s, plugins %s"
             (thither-get i "name") host (thither-get i "daemon_pid")
             (if (numberp started) (format-time-string "%F %T" started) "?")
             (thither-get i "version") (or (thither-get i "version_skew") "none")
             (thither-get i "connections") (if (eql 1 (thither-get i "connections")) "" "s")
             (if plugins (mapconcat (lambda (p) (format "%s" p)) plugins ", ") "none"))
    i))

;;;###autoload
(defun thither-restart-session (host)
  "End HOST's thither-server session daemon; the next access starts a new one.
This ends everything the session runs on the host (its programs and,
later, terminals); open files are not affected.  Asks first."
  (interactive (list (thither--read-host "Restart the session of thither host: ")))
  (let* ((cs (thither--session-of host))
         (name (thither-get (cdr cs) "name")))
    (when (yes-or-no-p (format "Restart thither session %s on %s (ends its programs and terminals)? "
                               name host))
      (thither-call-sync (car cs) "session_shutdown" '(("timeout_ms" . 2000)))
      (puthash host t thither--session-restarting)
      (thither-disconnect host)
      (message "thither: session %s on %s ended; the next access starts a new one" name host)
      t)))

;;;; Tools that cannot work on remote files

(defun thither--remote-buffer-p ()
  (and buffer-file-name (thither--split buffer-file-name)))

(defun thither--skip-when-remote (orig &rest args)
  (unless (and thither-disable-remote-tools (thither--remote-buffer-p))
    (apply orig args)))

(defun thither--lsp-advice ()
  (dolist (f '(lsp lsp-deferred))
    (when (fboundp f) (advice-add f :around #'thither--skip-when-remote))))

(with-eval-after-load 'lsp-mode (thither--lsp-advice))
(with-eval-after-load 'flycheck
  (advice-add 'flycheck-may-enable-mode :around
              (lambda (orig &rest args)
                (and (not (and thither-disable-remote-tools (thither--remote-buffer-p)))
                     (apply orig args)))))

;;;; Searching: fall back when the host lacks rg / fd

(defun thither--remote-dir-p ()
  (and (thither--split default-directory) t))

(defun thither--fallback-advice (tool fallback)
  "Advice for a consult command: use FALLBACK on thither hosts without TOOL."
  (lambda (orig &rest args)
    (if (and (thither--remote-dir-p) (not (executable-find tool t)))
        (progn (message "thither: %s not found on the host, using %s" tool fallback)
               (apply fallback args))
      (apply orig args))))

(with-eval-after-load 'consult
  (advice-add 'consult-ripgrep :around (thither--fallback-advice "rg" #'consult-grep))
  (advice-add 'consult-fd :around (thither--fallback-advice "fd" #'consult-find)))

;;;; Enable

;; keep a value the user set before loading this file
(when (eq thither-command-function #'thither-default-command)
  (setq thither-command-function #'thither-launch-command))
(thither-fs-enable)

(provide 'thither-setup)
;;; thither-setup.el ends here
