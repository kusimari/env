;; ELPA PACKAGES
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  ;;For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)


(defun install-require (p)
  (when (not (package-installed-p p))
    (package-initialize)
    (package-refresh-contents)
    (package-install p)))
;; ELPA PACKAGES - END

(install-require 'tramp)


;; COLORS and VISUALS
(setq inhibit-startup-screen t) ;; no startup or splash

(global-hl-line-mode 1)
(install-require 'zenburn-theme)
(install-require 'solarized-theme)
(load-theme 'zenburn t)
(global-hl-line-mode 1)
;;(custom-set-faces `(highlight ((t (:background , "yellow")))))

(tool-bar-mode -1)
(menu-bar-mode -1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(global-linum-mode 1)
(setq linum-format "%3d ")
(setq column-number-mode t)
;; COLORS and VISUALS - END


;; DIRECTORY
;;(install-require 'sr-speedbar)
(install-require 'neotree)
(global-set-key (kbd "C-c C-n") 'neotree-toggle) ;; to close need to move out of neo buffer
;; DIRECTORY - END


;; SHELL PATH FROM SHELL !!
(install-require 'exec-path-from-shell) ;;   ;; install 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
;; SHELL PATH FROM SHELL !! - END


;; BACK UP SETTINGS
(setq vc-make-backup-files t)

(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.emacs.d/backup/per-save")))

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)

;; save tramp to local
(setq tramp-auto-save-directory "~/.emacs.d/backup/tramp-autosave")
;; BACK UP SETTINGS - END


;; PYTHON
;; (add-hook 'python-mode-hook '(lambda () (setq python-indent 4)))

;; (install-require 'flymake)
;; (install-require 'py-autopep8) ;;(require 'py-autopep8)
;; (add-hook 'before-save-hook 'py-autopep8-before-save)

;; (install-require 'virtualenvwrapper) ;;(require 'virtualenvwrapper)
;; (install-require 'nose) ;;(require 'nose)
;; (install-require 'jedi) ;;(require 'jedi)
;; (add-hook 'python-mode-hook 'jedi:setup)
;;   ;;(jedi:install-server)

;; (add-to-list 'package-archives
;;              '("elpy" . "http://jorgenschaefer.github.io/packages/"))
;; (install-require 'elpy) ;;(require 'elpy)
;; (add-hook 'python-mode-hook 'elpy-mode)
;; (add-hook 'python-mode-hook 'elpy-start)
;; (defun elpy-start ()
;;   (elpy-enable)
;;   (elpy-use-ipython)
;;   (elpy-set-test-runner 'elpy-test-nose-runner))
;; PYTHON - END


;; HASKELL
(install-require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)))

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;; HASKELL - END


;; LaTeX
(install-require 'auctex)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
;; LaTeX - END


;; SCALA
;;(install-require 'scala-mode2)
;;(install-require 'sbt-mode)
(install-require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;; SCALA - END

;; Markdown
(install-require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; Markdown
