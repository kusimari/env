;;; core-gittree.el --- Git diff viewing integration using my-use-package

;;; Commentary:
;; Git diff functionality integrated with core.el's my-use-package system.

;;; Code:

;; ============================================================
;; Package Dependencies
;; ============================================================

(my-use-package diffview
  :config
  (setq diffview-default-width 80
        diffview-always-split-horizontally t))

(my-use-package vdiff
  :config
  (setq vdiff-auto-refine t))

(my-use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally))

(my-use-package treemacs
  :config
  (setq treemacs-persist-file nil)
  (setq treemacs-width 45)
  (setq treemacs-width-is-initially-locked nil)
  (treemacs-git-mode 'deferred)
  (treemacs-git-commit-diff-mode t)
  (treemacs-indent-guide-mode t))

;; ============================================================
;; Global Variables
;; ============================================================

(defvar gittree--file-watches (make-hash-table :test 'equal)
  "Hash table mapping file paths to their file-notify watch descriptors.")

(defvar gittree--status-cache (make-hash-table :test 'equal)
  "Cache of git status for files. Maps filename to status string.")

(defvar gittree-original-visit-action nil
  "Store the original treemacs visit action.")

(defvar gittree--pending-refocus nil
  "When non-nil, vdiff post-refresh advice will return focus to treemacs.")

(defvar gittree--status-configs
  '(;; Clean/untracked files -> single panel
    (:pattern nil :left nil :right "working" :vdiff nil :desc "Clean file")
    (:pattern "^\\?\\?" :left nil :right "working" :vdiff nil :desc "Untracked")
    ;; Modified files -> dual panels
    (:pattern "^ M" :left "HEAD" :right "working" :vdiff t :desc "Modified in worktree")
    (:pattern "^M " :left "HEAD" :right ":0" :vdiff t :desc "Staged for commit")
    (:pattern "^MM" :left ":0" :right "working" :vdiff t :desc "Staged + modified")
    (:pattern "^ D" :left "HEAD" :right "empty" :vdiff t :desc "Deleted in worktree"))
  "Git status configurations. Each entry: pattern, left-ref, right-ref, use-vdiff, description.")

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
;; File Watch Functions
;; ============================================================

(defun gittree--file-changed-callback (event)
  "Handle file change EVENT and prompt to reload."
  (let* ((file (nth 2 event))
         (action (nth 1 event))
         (buf (get-file-buffer file)))
    (when (and buf
               (memq action '(changed))
               (buffer-live-p buf)
               (not (buffer-modified-p buf)))
      (with-current-buffer buf
        (message "GitTree: File %s changed externally" (buffer-name))
        (if (y-or-n-p (format "File %s changed on disk. Reload? " (buffer-name)))
            (revert-buffer t t t)
          (message "GitTree: Keeping buffer as-is"))))))

(defun gittree-watch-file ()
  "Set up file watcher for current buffer."
  (when (and (buffer-file-name)
             (file-exists-p (buffer-file-name))
             (not (gethash (buffer-file-name) gittree--file-watches)))
    (let ((descriptor (file-notify-add-watch
                       (buffer-file-name)
                       '(change)
                       #'gittree--file-changed-callback)))
      (puthash (buffer-file-name) descriptor gittree--file-watches))))

(defun gittree-unwatch-file ()
  "Remove file watcher for current buffer."
  (when-let ((descriptor (gethash (buffer-file-name) gittree--file-watches)))
    (file-notify-rm-watch descriptor)
    (remhash (buffer-file-name) gittree--file-watches)))

;; ============================================================
;; Commit Comparison Functions
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
      (with-current-buffer (get-buffer-create buffer1-name)
        (erase-buffer)
        (let ((git-cmd (format "git show %s:%s" commit1 relative-path)))
          (call-process-shell-command git-cmd nil t nil))
        (set-buffer-modified-p nil)
        (read-only-mode 1)
        (when (fboundp 'display-line-numbers-mode)
          (display-line-numbers-mode 1)))
      (with-current-buffer (get-buffer-create buffer2-name)
        (erase-buffer)
        (let ((git-cmd (format "git show %s:%s" commit2 relative-path)))
          (call-process-shell-command git-cmd nil t nil))
        (set-buffer-modified-p nil)
        (read-only-mode 1)
        (when (fboundp 'display-line-numbers-mode)
          (display-line-numbers-mode 1)))
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
    (find-file file-path)
    (when (fboundp 'display-line-numbers-mode)
      (display-line-numbers-mode 1))
    (with-current-buffer (get-buffer-create commit-buffer-name)
      (erase-buffer)
      (let ((git-cmd (format "git show %s:%s" commit relative-path)))
        (call-process-shell-command git-cmd nil t nil))
      (set-buffer-modified-p nil)
      (read-only-mode 1)
      (when (fboundp 'display-line-numbers-mode)
        (display-line-numbers-mode 1)))
    (vdiff-buffers (get-buffer commit-buffer-name) (current-buffer) nil t)
    (message "GitTree: VDiff active - n/p navigate hunks, C-c g/s get/send changes, q quit")))

(defun gittree-cleanup-file-buffers (file-path)
  "Clean up all git-related buffers for FILE-PATH to ensure fresh state."
  (let ((file-name (file-name-nondirectory file-path))
        (buffers-killed 0))
    (when (and (fboundp 'vdiff-quit)
               (bound-and-true-p vdiff-mode))
      (vdiff-quit)
      (message "GitTree: Closed vdiff session"))
    (dolist (buffer (buffer-list))
      (let ((buffer-name (buffer-name buffer)))
        (when (and buffer-name
                   (or (string= buffer-name (format "*%s@HEAD*" file-name))
                       (string= buffer-name (format "*%s@staged*" file-name))
                       (string-prefix-p (format "*%s@" file-name) buffer-name)))
          (message "GitTree: Killing buffer %s" buffer-name)
          (kill-buffer buffer)
          (setq buffers-killed (1+ buffers-killed)))))
    (when (> buffers-killed 0)
      (message "GitTree: Cleaned up %d buffer(s) for %s" buffers-killed file-name))))

;; ============================================================
;; Buffer Creation Helpers
;; ============================================================

(defun gittree--create-git-buffer (file-path git-ref display-name)
  "Create buffer with git content from GIT-REF for FILE-PATH."
  (let* ((git-root (gittree--git-root file-path))
         (relative-path (file-relative-name file-path git-root))
         (file-name (file-name-nondirectory file-path))
         (buffer-name (format "*%s@%s*" file-name display-name))
         (default-directory git-root))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (call-process-shell-command (format "git show %s:%s" git-ref relative-path) nil t nil)
      (let ((buffer-file-name file-path)) (set-auto-mode))
      (set-buffer-modified-p nil)
      (read-only-mode 1)
      (when (fboundp 'display-line-numbers-mode) (display-line-numbers-mode 1))
      (current-buffer))))

(defun gittree--create-empty-buffer (file-name)
  "Create empty read-only buffer for FILE-NAME."
  (with-current-buffer (get-buffer-create (format "*%s (empty)*" file-name))
    (erase-buffer)
    (read-only-mode 1)
    (current-buffer)))

(defun gittree--prepare-working-file (file-path)
  "Open working file and set up display."
  (find-file file-path)
  (when (fboundp 'display-line-numbers-mode) (display-line-numbers-mode 1))
  (current-buffer))

(defun gittree--ref-display-name (ref)
  "Get display name for REF."
  (cond
   ((string= ref "HEAD") "HEAD")
   ((string= ref ":0") "staged")
   ((string= ref "working") "working")
   ((string= ref "empty") "empty")
   (t ref)))

(defun gittree--create-buffer (file-path ref)
  "Create appropriate buffer for REF type."
  (cond
   ((string= ref "HEAD") (gittree--create-git-buffer file-path "HEAD" "HEAD"))
   ((string= ref ":0") (gittree--create-git-buffer file-path ":" "staged"))
   ((string= ref "working") (gittree--prepare-working-file file-path))
   ((string= ref "empty") (gittree--create-empty-buffer (file-name-nondirectory file-path)))
   (t (error "Unknown ref: %s" ref))))

;; ============================================================
;; Panel Display Functions
;; ============================================================

(defun gittree-show-dual-panel (file-path left-ref right-ref use-vdiff)
  "Compare LEFT-REF vs RIGHT-REF for FILE-PATH, optionally using vdiff.
Refs can be 'HEAD', ':0' (staged), 'working', 'empty', or git references."
  (let* ((file-path (expand-file-name file-path))
         (left-buffer (gittree--create-buffer file-path left-ref))
         (right-buffer (gittree--create-buffer file-path right-ref))
         (left-name (gittree--ref-display-name left-ref))
         (right-name (gittree--ref-display-name right-ref)))
    (split-window-right)
    (switch-to-buffer left-buffer)
    (other-window 1)
    (switch-to-buffer right-buffer)
    (if use-vdiff
        (progn
          (setq gittree--pending-refocus t)
          (vdiff-buffers left-buffer right-buffer nil t)
          (message "GitTree: VDiff active - %s | %s" left-name right-name))
      (message "GitTree: Split view - %s | %s" left-name right-name))))

(defun gittree--refocus-treemacs (&rest _)
  "Return focus to treemacs after vdiff finishes setup."
  (when gittree--pending-refocus
    (setq gittree--pending-refocus nil)
    (when-let ((tw (treemacs-get-local-window)))
      (select-window tw))))

(defun gittree-show-single-panel (path)
  "Show single panel with working file at PATH for editing."
  (find-file path)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode 1)))

(defun gittree-cleanup-panels ()
  "Clean up content area to single panel with *scratch* buffer."
  (let ((treemacs-win (treemacs-get-local-window)))
    (when treemacs-win
      (let ((content-windows '()))
        (dolist (win (window-list))
          (unless (eq win treemacs-win)
            (push win content-windows)))
        (when content-windows
          (let ((keep-win (car content-windows)))
            (dolist (win (cdr content-windows))
              (delete-window win))
            (select-window keep-win)
            (switch-to-buffer "*scratch*")))
        (unless content-windows
          (select-window treemacs-win)
          (split-window-right)
          (other-window 1)
          (switch-to-buffer "*scratch*"))))))

;; ============================================================
;; Git Status Functions
;; ============================================================

(defun gittree--get-file-git-status (filepath)
  "Get git status string for FILEPATH using git status --porcelain.
Returns the raw status string (e.g., ' M', 'MM', '??') or nil if no changes."
  (let* ((git-root (gittree--git-root filepath))
         (relative-path (file-relative-name filepath git-root))
         (default-directory git-root))
    (when (and git-root (file-exists-p filepath))
      (with-temp-buffer
        (when (= 0 (call-process "git" nil t nil "status" "--porcelain" "--" relative-path))
          (let ((output (string-trim-right (buffer-string))))
            (if (string-empty-p output) nil output)))))))

(defun gittree--find-status-config (git-status)
  "Find configuration entry for GIT-STATUS."
  (or (seq-find (lambda (config)
                  (let ((pattern (plist-get config :pattern)))
                    (if pattern
                        (and git-status (string-match pattern git-status))
                      (not git-status))))
                gittree--status-configs)
      '(:pattern nil :left nil :right "working" :vdiff nil :desc "Unknown status")))

(defun gittree--status-to-prefix (git-status)
  "Convert GIT-STATUS string to 4-char prefix for display."
  (if (and git-status (>= (length git-status) 2))
      (format "%s  " (substring git-status 0 2))
    "    "))

(defun gittree--get-all-git-status ()
  "Get git status for all files in current project.
Returns alist of (filepath . status-string)."
  (let ((git-root (gittree--git-root default-directory))
        (results nil))
    (when git-root
      (let ((default-directory git-root))
        (with-temp-buffer
          (when (= 0 (call-process "git" nil t nil "status" "--porcelain" "-uall"))
            (goto-char (point-min))
            (while (not (eobp))
              (let* ((line (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))
                     (status (and (>= (length line) 2) (substring line 0 2)))
                     (filename (and (>= (length line) 3) (substring line 3)))
                     (filepath (and filename (expand-file-name filename git-root))))
                (when (and filepath status)
                  (push (cons filepath status) results)))
              (forward-line 1))))))
    results))

(defun gittree--file-name-transformer (filename)
  "Transform FILENAME to include git status prefix."
  (let* ((status (gethash filename gittree--status-cache))
         (prefix (gittree--status-to-prefix status)))
    (concat prefix filename)))

(defun gittree-refresh-status ()
  "Refresh git status cache and treemacs display."
  (interactive)
  (clrhash gittree--status-cache)
  (let ((status-alist (gittree--get-all-git-status))
        (count 0))
    (dolist (item status-alist)
      (let ((filename (file-name-nondirectory (car item)))
            (status (cdr item)))
        (puthash filename status gittree--status-cache)
        (setq count (1+ count))))
    (when (treemacs-get-local-window)
      (treemacs-run-in-every-buffer
       (treemacs--do-refresh (current-buffer) 'all))
      (message "GitTree: %d files with changes" count))))

;; ============================================================
;; Visit Node Handler
;; ============================================================

(defun gittree-visit-node (&optional arg)
  "Visit node with git-aware panel logic."
  (let ((state (treemacs--prop-at-point :state))
        (path (treemacs--prop-at-point :path)))
    (cond
     ((and (memq state '(file-node-open file-node-closed))
           path
           (file-regular-p path))
      (let* ((git-status (gittree--get-file-git-status path))
             (config (gittree--find-status-config git-status))
             (filename (file-name-nondirectory path))
             (treemacs-win (treemacs-get-local-window))
             (left-ref (plist-get config :left))
             (right-ref (plist-get config :right))
             (use-vdiff (plist-get config :vdiff))
             (desc (plist-get config :desc)))
        (message "GitTree: %s | status='%s' | %s" filename git-status desc)
        (gittree-cleanup-panels)
        (gittree-cleanup-file-buffers path)
        (if left-ref
            (gittree-show-dual-panel path left-ref right-ref use-vdiff)
          (progn
            (gittree-show-single-panel path)
            (when treemacs-win (select-window treemacs-win))))))
     (t
      (when gittree-original-visit-action
        (funcall gittree-original-visit-action arg))))))

;; ============================================================
;; Mode Enable/Disable
;; ============================================================

(defun gittree--enable ()
  "Enable gittree interface."
  ;; Start treemacs
  (treemacs-add-project-to-workspace default-directory
                                      (file-name-nondirectory (directory-file-name default-directory)))
  (treemacs)

  ;; Apply customizations
  (setq treemacs-file-name-transformer #'gittree--file-name-transformer)
  (setq gittree-original-visit-action treemacs-default-visit-action)
  (setq treemacs-default-visit-action #'gittree-visit-node)

  ;; Set up hooks
  (add-hook 'find-file-hook #'gittree-watch-file)
  (add-hook 'kill-buffer-hook #'gittree-unwatch-file)

  ;; Advice for focus management after vdiff setup
  (advice-add 'vdiff--diff-refresh-finish :after #'gittree--refocus-treemacs)

  ;; Keybinding
  (with-eval-after-load 'treemacs
    (define-key treemacs-mode-map (kbd "g") #'gittree-refresh-status))

  ;; Initial refresh
  (run-with-idle-timer 0.5 nil #'gittree-refresh-status)

  (message "GitTree: Interface activated"))

(defun gittree--disable ()
  "Disable gittree interface."
  ;; Close treemacs
  (when (treemacs-get-local-window)
    (treemacs-quit))

  ;; Restore treemacs defaults
  (when gittree-original-visit-action
    (setq treemacs-default-visit-action gittree-original-visit-action))
  (setq treemacs-file-name-transformer nil)

  ;; Remove hooks
  (remove-hook 'find-file-hook #'gittree-watch-file)
  (remove-hook 'kill-buffer-hook #'gittree-unwatch-file)

  ;; Remove vdiff advice
  (advice-remove 'vdiff--diff-refresh-finish #'gittree--refocus-treemacs)

  ;; Clean up file watches
  (maphash (lambda (_file descriptor)
             (file-notify-rm-watch descriptor))
           gittree--file-watches)
  (clrhash gittree--file-watches)

  ;; Clean up status cache
  (clrhash gittree--status-cache)

  (message "GitTree: Interface deactivated"))

;; ============================================================
;; Minor Mode Definition
;; ============================================================

;;;###autoload
(define-minor-mode gittree-mode
  "Global minor mode for git-aware file tree interface."
  :global t
  :lighter " GitTree"
  (if gittree-mode
      (gittree--enable)
    (gittree--disable)))

;;;###autoload
(defun gittree ()
  "Start GitTree interface."
  (interactive)
  (gittree-mode 1))

;; Legacy function for backward compatibility
(defun activate-gittree-integration ()
  "Legacy function - use gittree-mode instead."
  (gittree-mode 1))

(provide 'core-gittree)
;;; core-gittree.el ends here
