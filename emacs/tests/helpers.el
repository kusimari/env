;;; helpers.el --- Test helpers for gittree batch tests -*- lexical-binding: t -*-

;;; Commentary:
;; Lightweight test harness for core-gittree.el. Runs in `emacs --batch`
;; so the elisp under test can be exercised without a live display, a
;; user home-manager switch, or MELPA installs.
;;
;; Treemacs and vdiff are stubbed so the gittree entry points can reach
;; the buffer-creation + git paths we actually care about.

;;; Code:

(require 'cl-lib)

(defvar gittree-test--cases nil
  "Reverse-ordered list of registered (NAME . THUNK) test cases.")

(defvar gittree-test--failures nil
  "Reverse-ordered list of (NAME . MESSAGE) for failures.")

(defmacro gittree-test-deftest (name &rest body)
  "Register BODY as a test case named NAME (a symbol)."
  (declare (indent 1))
  `(push (cons ',name (lambda () ,@body)) gittree-test--cases))

(defun gittree-test-assert (condition message)
  "Signal a test failure with MESSAGE when CONDITION is nil."
  (unless condition
    (error "%s" message)))

(defun gittree-test-assert-equal (actual expected message)
  "Assert ACTUAL equals EXPECTED; otherwise fail with MESSAGE."
  (unless (equal actual expected)
    (error "%s\n  expected: %S\n  actual:   %S" message expected actual)))

(defun gittree-test-assert-match (regex string message)
  "Assert REGEX matches STRING; otherwise fail with MESSAGE."
  (unless (and (stringp string) (string-match-p regex string))
    (error "%s\n  regex:   %s\n  string:  %S" message regex string)))

(defun gittree-test-assert-no-match (regex string message)
  "Assert REGEX does NOT match STRING; otherwise fail with MESSAGE."
  (when (and (stringp string) (string-match-p regex string))
    (error "%s\n  regex:   %s\n  string:  %S" message regex string)))

(defun gittree-test--run-git (&rest args)
  "Run git with ARGS in `default-directory', erroring on non-zero exit."
  (let ((status (apply #'call-process "git" nil nil nil args)))
    (unless (zerop status)
      (error "git %s failed with status %s" (string-join args " ") status))))

(defun gittree-test--git-output (&rest args)
  "Run git with ARGS, return stdout as a trimmed string."
  (with-temp-buffer
    (let ((status (apply #'call-process "git" nil t nil args)))
      (unless (zerop status)
        (error "git %s failed with status %s" (string-join args " ") status))
      (string-trim (buffer-string)))))

(defun gittree-test-make-fixture-repo (root)
  "Build a two-commit fixture repo at ROOT; return a plist of SHAs.

  Commit 1: alpha.md (v1), beta.md, dir/shared.md
  Commit 2: modify alpha.md, delete beta.md, add gamma.md, add dir/other.md

Returns (:commit1 SHA1 :commit2 SHA2)."
  (let ((default-directory (file-name-as-directory root)))
    (gittree-test--run-git "init" "-q" "-b" "main")
    (gittree-test--run-git "config" "user.email" "test@example.com")
    (gittree-test--run-git "config" "user.name" "test")
    (gittree-test--run-git "config" "commit.gpgsign" "false")
    ;; Commit 1
    (with-temp-file "alpha.md" (insert "alpha v1\nline two\n"))
    (with-temp-file "beta.md" (insert "beta content\n"))
    (make-directory "dir" t)
    (with-temp-file "dir/shared.md" (insert "shared v1\n"))
    (gittree-test--run-git "add" "-A")
    (gittree-test--run-git "commit" "-q" "-m" "first")
    (let ((sha1 (gittree-test--git-output "rev-parse" "HEAD")))
      ;; Commit 2
      (with-temp-file "alpha.md" (insert "alpha v2 modified\nline two\nthird line\n"))
      (delete-file "beta.md")
      (with-temp-file "gamma.md" (insert "gamma new file\n"))
      (with-temp-file "dir/other.md" (insert "other new\n"))
      (gittree-test--run-git "add" "-A")
      (gittree-test--run-git "commit" "-q" "-m" "second")
      (let ((sha2 (gittree-test--git-output "rev-parse" "HEAD")))
        (list :commit1 sha1 :commit2 sha2)))))

;; ============================================================
;; Stubs for treemacs + vdiff so gittree-mode can run headless.
;;
;; Only covers the surface gittree--enable / gittree-launch actually
;; touches in batch mode. Real behavior is exercised on a live display;
;; these stubs let us verify the buffer/content paths in isolation.
;; ============================================================

(defvar gittree-test--stub-treemacs-window nil
  "Faux window object used by treemacs stubs.")

(defun gittree-test--install-stubs ()
  "Install stubs for treemacs/vdiff so gittree can run in batch."
  ;; Pretend no treemacs window exists in batch — gittree-cleanup-panels
  ;; and gittree--refocus-treemacs become no-ops, which is fine for the
  ;; buffer-level assertions we care about.
  (fset 'treemacs-get-local-window (lambda () nil))
  (fset 'treemacs-add-project-to-workspace (lambda (&rest _) nil))
  (fset 'treemacs (lambda (&rest _) nil))
  (fset 'treemacs-quit (lambda () nil))
  (fset 'treemacs--do-refresh (lambda (&rest _) nil))
  (fset 'treemacs-run-in-every-buffer (lambda (&rest _) nil))
  (defvar treemacs-mode-map (make-sparse-keymap))
  (defvar treemacs-file-name-transformer nil)
  (defvar treemacs-default-visit-action nil)
  ;; vdiff calls are no-ops; we assert on buffer contents, not vdiff UI.
  (fset 'vdiff-buffers (lambda (&rest _) nil))
  (fset 'vdiff-quit (lambda () nil))
  (defvar vdiff-mode nil)
  ;; ediff used by gittree-compare-commits (not on the emacs-gittree
  ;; path but we want the stub so the file loads cleanly).
  (fset 'ediff-buffers (lambda (&rest _) nil)))

(defun gittree-run-tests ()
  "Run every registered test case. kill-emacs with non-zero on any failure."
  (let ((cases (reverse gittree-test--cases))
        (passed 0))
    (dolist (case cases)
      (let ((name (car case))
            (thunk (cdr case)))
        (condition-case err
            (progn
              (funcall thunk)
              (setq passed (1+ passed))
              (message "PASS: %s" name))
          (error
           (push (cons name (error-message-string err))
                 gittree-test--failures)
           (message "FAIL: %s: %s" name (error-message-string err))))))
    (message "---")
    (message "%d passed, %d failed"
             passed (length gittree-test--failures))
    (if gittree-test--failures
        (kill-emacs 1)
      (kill-emacs 0))))

(provide 'helpers)
;;; helpers.el ends here
