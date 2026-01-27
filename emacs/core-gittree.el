;;; core-gittree.el --- Simple magit setup with external file change watcher

;;; Commentary:
;; Simple magit setup with file watcher for external changes.
;; Designed to work with lazygit → emacs integration.

;;; Code:

(defun activate-gittree-integration ()
  "Set up basic magit with external file change watcher."

  ;; Install and configure magit (simple setup)
  (my-use-package magit
    :config
    ;; Basic magit configuration
    (setq magit-refresh-status-buffer t
          magit-diff-refine-hunk 'all))

  ;; External file change handler
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

  (message "GitTree: File watcher setup loaded"))

(provide 'core-gittree)
;;; core-gittree.el ends here