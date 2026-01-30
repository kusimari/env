;;; core-gittree.el --- Git diff viewing integration using my-use-package

;;; Commentary:
;; Git diff functionality integrated with core.el's my-use-package system.
;; Provides enhanced diff viewing for lazygit integration with proper package management.

;;; Code:

(defun activate-gittree-integration ()
  "Activate gittree integration with proper package setup."

  ;; Install diffview for GitHub-style side-by-side diffs
  (my-use-package diffview
    :config
    (setq diffview-default-width 80
          diffview-always-split-horizontally t))

  ;; Install vdiff for editable side-by-side comparisons
  (my-use-package vdiff
    :config
    (setq vdiff-default-refinement-syntax-code 't
          vdiff-auto-refine t))

  ;; Configure ediff for side-by-side display
  (my-use-package ediff
    :config
    ;; Side-by-side layout preferences
    (setq ediff-window-setup-function 'ediff-setup-windows-plain
          ediff-split-window-function 'split-window-horizontally
          ediff-merge-split-window-function 'split-window-horizontally))

  ;; External file change handler for file watching
  (defun gittree-handle-file-change ()
    "Handle external file changes - ask user to keep or refresh."
    (when (and (buffer-file-name)
               (not (verify-visited-file-modtime (current-buffer)))
               (file-exists-p (buffer-file-name)))
      (let ((choice (read-char-choice
                     (format "File %s changed externally. (k)eep current, (r)efresh from disk: "
                             (buffer-name))
                     '(?k ?r))))
        (cond
         ((eq choice ?k)
          (message "Keeping current version of %s" (buffer-name)))
         ((eq choice ?r)
          (revert-buffer t t t)
          (message "Refreshed %s from disk" (buffer-name)))))))

  ;; Set up file watcher for external changes
  (defun gittree-setup-file-watcher ()
    "Set up file change watcher for current buffer."
    (when (and (buffer-file-name)
               (vc-backend (buffer-file-name)))
      ;; Enable auto-revert mode but customize the behavior
      (auto-revert-mode 1)
      (setq-local auto-revert-use-notify t)
      ;; Override the default auto-revert behavior
      (setq-local revert-buffer-function 'gittree-handle-file-change)))

  ;; Apply file watcher to git files
  (add-hook 'find-file-hook 'gittree-setup-file-watcher)

  (message "GitTree: Integration activated with diffview, vdiff, ediff, and file watching"))

;; ============================================================
;; Git Helper Functions
;; ============================================================

(defun gittree--git-root (file-path)
  "Find git root for FILE-PATH."
  (or (vc-find-root file-path ".git") default-directory))

(defun gittree--short-commit (commit)
  "Return short version of COMMIT (max 7 chars)."
  (substring commit 0 (min 7 (length commit))))

;; ============================================================
;; Core Diff Functions
;; ============================================================

(defun gittree-compare-commits (file-path commit1 commit2)
  "Compare FILE-PATH between COMMIT1 and COMMIT2 using ediff.
Creates side-by-side comparison with file contents at each commit."
  (interactive "fFile: \nsFirst commit: \nsSecond commit: ")

  (if (string= commit1 commit2)
      (message "Warning: Both commits are the same (%s). No diff to show." commit1)

    (let* ((file-path (expand-file-name file-path))
           (git-root (gittree--git-root file-path))
           (relative-path (file-relative-name file-path git-root))
           (file-name (file-name-nondirectory file-path))
           (buffer1-name (format "*%s@%s*" file-name (gittree--short-commit commit1)))
           (buffer2-name (format "*%s@%s*" file-name (gittree--short-commit commit2)))
           (default-directory git-root))

      ;; Get file contents at commit1
      (with-current-buffer (get-buffer-create buffer1-name)
        (erase-buffer)
        (let ((git-cmd (format "git show %s:%s" commit1 relative-path)))
          (call-process-shell-command git-cmd nil t nil))
        (set-buffer-modified-p nil)
        (read-only-mode 1)
        (when (fboundp 'display-line-numbers-mode)
          (display-line-numbers-mode 1)))

      ;; Get file contents at commit2
      (with-current-buffer (get-buffer-create buffer2-name)
        (erase-buffer)
        (let ((git-cmd (format "git show %s:%s" commit2 relative-path)))
          (call-process-shell-command git-cmd nil t nil))
        (set-buffer-modified-p nil)
        (read-only-mode 1)
        (when (fboundp 'display-line-numbers-mode)
          (display-line-numbers-mode 1)))

      ;; Launch ediff side-by-side comparison
      (ediff-buffers (get-buffer buffer1-name) (get-buffer buffer2-name))
      (message "GitTree: Ediff active - n/p navigate diffs, ? help, q quit"))))

(defun gittree-compare-working (file-path commit)
  "Compare working FILE-PATH against COMMIT using vdiff.
Left: commit version (read-only), Right: working file (editable)."
  (interactive "fFile: \nsCompare against commit: ")

  (let* ((file-path (expand-file-name file-path))
         (git-root (gittree--git-root file-path))
         (relative-path (file-relative-name file-path git-root))
         (file-name (file-name-nondirectory file-path))
         (commit-buffer-name (format "*%s@%s*" file-name (gittree--short-commit commit)))
         (default-directory git-root))

    ;; Open the working file (editable)
    (find-file file-path)
    (when (fboundp 'display-line-numbers-mode)
      (display-line-numbers-mode 1))

    ;; Get file contents at commit (read-only)
    (with-current-buffer (get-buffer-create commit-buffer-name)
      (erase-buffer)
      (let ((git-cmd (format "git show %s:%s" commit relative-path)))
        (call-process-shell-command git-cmd nil t nil))
      (set-buffer-modified-p nil)
      (read-only-mode 1)
      (when (fboundp 'display-line-numbers-mode)
        (display-line-numbers-mode 1)))

    ;; Launch vdiff - commit on left (read-only), working file on right (editable)
    (vdiff-buffers (get-buffer commit-buffer-name) (current-buffer) nil t)
    (message "GitTree: VDiff active - n/p navigate hunks, C-c g/s get/send changes, q quit")))


(provide 'core-gittree)
;;; core-gittree.el ends here
