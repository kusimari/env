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

(defvar gittree--file-modtimes (make-hash-table :test 'equal)
  "Hash table mapping watched file paths to their last-known modification times.")

(defvar gittree--poll-timer nil
  "Timer for polling file modification times.")

(defvar gittree--status-cache (make-hash-table :test 'equal)
  "Cache of git status for files. Maps filename to status string.")

(defvar gittree-original-visit-action nil
  "Store the original treemacs visit action.")

(defvar gittree--pending-refocus nil
  "When non-nil, vdiff post-refresh advice will return focus to treemacs.")

(defvar gittree-launch-left-ref nil
  "When non-nil, overrides the status-based left ref in `gittree-visit-node'.
Set by `gittree-launch' so interactively-selected files get compared at the
user-supplied refs instead of status-derived ones.")

(defvar gittree-launch-right-ref nil
  "When non-nil, overrides the status-based right ref in `gittree-visit-node'.")

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
;; File Change Polling
;; ============================================================

(defun gittree--check-file-changes ()
  "Poll watched files for external modifications and prompt to reload.
Skips check when minibuffer is active or during keyboard macros to
avoid disrupting user input."
  (unless (or (active-minibuffer-window)
              executing-kbd-macro)
    (maphash
     (lambda (file saved-modtime)
       (when (file-exists-p file)
         (let ((current-modtime (file-attribute-modification-time
                                 (file-attributes file))))
           (when (and current-modtime
                      (not (time-equal-p saved-modtime current-modtime)))
             (puthash file current-modtime gittree--file-modtimes)
             (let ((buf (find-buffer-visiting file)))
               (when (and buf
                          (buffer-live-p buf)
                          (not (buffer-modified-p buf)))
                 (with-current-buffer buf
                   (if (y-or-n-p (format "File %s changed on disk. Reload? "
                                         (buffer-name)))
                       (revert-buffer t t t)
                     (message "GitTree: Keeping buffer as-is")))))))))
     gittree--file-modtimes)))

(defun gittree-watch-file ()
  "Record current buffer's file modtime for change tracking."
  (when-let ((file (buffer-file-name)))
    (when (and (file-exists-p file)
               (not (gethash file gittree--file-modtimes)))
      (puthash file
               (file-attribute-modification-time (file-attributes file))
               gittree--file-modtimes))))

(defun gittree-unwatch-file ()
  "Stop tracking current buffer's file."
  (when-let ((file (buffer-file-name)))
    (remhash file gittree--file-modtimes)))

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
        (call-process "git" nil t nil "show" (format "%s:%s" commit1 relative-path))
        (set-buffer-modified-p nil)
        (read-only-mode 1)
        (when (fboundp 'display-line-numbers-mode)
          (display-line-numbers-mode 1)))
      (with-current-buffer (get-buffer-create buffer2-name)
        (erase-buffer)
        (call-process "git" nil t nil "show" (format "%s:%s" commit2 relative-path))
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
      (call-process "git" nil t nil "show" (format "%s:%s" commit relative-path))
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
      ;; Direct call-process (not call-process-shell-command) — a shell here
      ;; would run interactive zsh init (atuin, oh-my-zsh, etc.) which tries
      ;; to create state dirs and fails when PWD is a nix store path.
      (call-process "git" nil t nil "show" (format "%s:%s" git-ref relative-path))
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
  "Create appropriate buffer for REF.
REF may be:
  `working' — the file on disk
  `:0'      — the staged version (git index)
  `empty'   — an empty buffer (for deleted files)
  any other string — treated as a git ref (commit SHA, branch, tag,
                     HEAD, HEAD~N, etc.) and passed to
                     `git show <ref>:<path>'."
  (cond
   ((string= ref ":0") (gittree--create-git-buffer file-path ":" "staged"))
   ((string= ref "working") (gittree--prepare-working-file file-path))
   ((string= ref "empty") (gittree--create-empty-buffer (file-name-nondirectory file-path)))
   (t (gittree--create-git-buffer file-path ref ref))))

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

(defun gittree--get-ref-diff-status (ref-a ref-b)
  "Get files changed between REF-A and REF-B via `git diff --name-status'.
Returns alist of (filepath . status-string) where status-string is a
two-char porcelain-style prefix (e.g., ` M' for modified, ` A' for
added, ` D' for deleted) so it renders via `gittree--status-to-prefix'."
  (let ((git-root (gittree--git-root default-directory))
        (results nil))
    (when (and git-root ref-a ref-b
               (not (string= ref-a "working"))
               (not (string= ref-b "working"))
               (not (string= ref-a ":0"))
               (not (string= ref-b ":0")))
      (let ((default-directory git-root))
        (with-temp-buffer
          (when (= 0 (call-process "git" nil t nil
                                   "diff" "--name-status"
                                   ref-a ref-b))
            (goto-char (point-min))
            (while (not (eobp))
              (let* ((line (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))
                     (parts (split-string line "\t" t))
                     (code (car parts))
                     ;; --name-status emits rename/copy with two paths; take the last.
                     (filename (car (last parts)))
                     (status (pcase (and code (substring code 0 1))
                               ("A" " A")
                               ("M" " M")
                               ("D" " D")
                               ("R" " R")
                               ("C" " C")
                               ("T" " T")
                               (_ "  ")))
                     (filepath (and filename (expand-file-name filename git-root))))
                (when filepath
                  (push (cons filepath status) results)))
              (forward-line 1))))))
    results))

(defun gittree--file-name-transformer (filename)
  "Transform FILENAME to include git status prefix."
  (let* ((status (gethash filename gittree--status-cache))
         (prefix (gittree--status-to-prefix status)))
    (concat prefix filename)))

(defun gittree-refresh-status ()
  "Refresh git status cache and treemacs display.
When launch-refs are active, source file-change status from
`git diff <ref-a> <ref-b>' instead of working-tree `git status'."
  (interactive)
  (clrhash gittree--status-cache)
  (let* ((launch-refs (and gittree-launch-left-ref
                           gittree-launch-right-ref
                           (not (string= gittree-launch-left-ref "working"))
                           (not (string= gittree-launch-right-ref "working"))))
         (status-alist (if launch-refs
                           (gittree--get-ref-diff-status
                            gittree-launch-left-ref
                            gittree-launch-right-ref)
                         (gittree--get-all-git-status)))
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
             (override (or gittree-launch-left-ref gittree-launch-right-ref))
             (left-ref (or gittree-launch-left-ref (plist-get config :left)))
             (right-ref (or gittree-launch-right-ref (plist-get config :right)))
             (use-vdiff (if override t (plist-get config :vdiff)))
             (desc (if override
                       (format "Launch: %s vs %s" left-ref right-ref)
                     (plist-get config :desc))))
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

  ;; Disable line numbers in treemacs buffer (overrides global-display-line-numbers-mode)
  (when-let ((tw (treemacs-get-local-window)))
    (with-current-buffer (window-buffer tw)
      (setq-local display-line-numbers nil)))

  ;; Apply customizations
  (setq treemacs-file-name-transformer #'gittree--file-name-transformer)
  (setq gittree-original-visit-action treemacs-default-visit-action)
  (setq treemacs-default-visit-action #'gittree-visit-node)

  ;; Set up hooks and polling for file change detection
  (add-hook 'find-file-hook #'gittree-watch-file)
  (add-hook 'kill-buffer-hook #'gittree-unwatch-file)
  (setq gittree--poll-timer (run-with-timer 2 2 #'gittree--check-file-changes))

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

  ;; Cancel poll timer and clean up tracked files
  (when gittree--poll-timer
    (cancel-timer gittree--poll-timer)
    (setq gittree--poll-timer nil))
  (clrhash gittree--file-modtimes)

  ;; Clean up status cache
  (clrhash gittree--status-cache)

  ;; Clear any launch overrides
  (setq gittree-launch-left-ref nil
        gittree-launch-right-ref nil)

  (message "GitTree: Interface deactivated"))

;; ============================================================
;; CLI Launch Entry Point
;; ============================================================

;;;###autoload
(defun gittree-launch (left-ref right-ref &optional file)
  "Start gittree-mode with LEFT-REF vs RIGHT-REF diff for FILE.
When FILE is nil, gittree-mode opens with the tree active and the tree
is populated from `git diff --name-status LEFT-REF RIGHT-REF'; the user
picks a file and the right panel shows the LEFT-REF vs RIGHT-REF diff.
When FILE is non-nil, also open that file's diff immediately.
Either ref may be 'working', ':0', or any git ref (HEAD, HEAD~N, SHA,
branch, tag — passed to `git show <ref>:<path>')."
  (setq gittree-launch-left-ref (and left-ref (not (string-empty-p left-ref)) left-ref)
        gittree-launch-right-ref (and right-ref (not (string-empty-p right-ref)) right-ref))
  (gittree-mode 1)
  (message "GitTree: launched with %s vs %s"
           gittree-launch-left-ref gittree-launch-right-ref)
  (when (and file (not (string-empty-p file)))
    (let ((expanded (expand-file-name file)))
      (when (file-exists-p expanded)
        (gittree-cleanup-panels)
        (gittree-cleanup-file-buffers expanded)
        (gittree-show-dual-panel
         expanded
         gittree-launch-left-ref
         gittree-launch-right-ref
         t)))))

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
