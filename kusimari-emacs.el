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
    (package-install p))
  (require p))
;; ELPA PACKAGES - END

(install-require 'tramp)

;; MOUSE && SCROLL
(defun my-bell-function ()
  (unless (memq this-command
                '(scroll-up-line scroll-down-line))
    (ding)))
(setq ring-bell-function 'my-bell-function)

(xterm-mouse-mode t)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)
;; MOUSE && SCROLL - END

;; COLORS and VISUALS
(setq inhibit-startup-screen t) ;; no startup or splash

(install-require 'zenburn-theme)
(load-theme 'zenburn t)
(global-hl-line-mode 1)
(setq flycheck-highlighting-mode 'lines)
(zenburn-with-color-variables
  (custom-theme-set-faces
   'zenburn
   `(hl-line ((t (:background ,zenburn-bg+1 :foreground nil))))
   `(flycheck-error ((t :background ,zenburn-red-4 :foreground nil)))))

(tool-bar-mode -1)
(menu-bar-mode -1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(global-linum-mode 1)
(setq linum-format "%3d ")
(setq column-number-mode t)
;; COLORS and VISUALS - END

;; EDITING
(install-require 'iedit)
(global-set-key (kbd "C-c k i") 'iedit-mode)
(global-set-key (kbd "C-c k /") 'comment-dwim)
;; EDITING - END


;; HELM, DIRECTORY and PROJECTS
;;(install-require 'sr-speedbar)
(install-require 'async)
(install-require 'helm)
(require 'helm-config)
(install-require 'projectile)
(install-require 'helm-projectile)
(install-require 'helm-descbinds)
(install-require 'helm-ls-git)

(setq helm-split-window-in-side-p           t
      helm-autoresize-max-height            25
      helm-autoresize-min-height            10)
(helm-autoresize-mode 1)
(helm-mode 1)
(helm-descbinds-mode)  ;; C-c C-h
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-c h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h d") 'helm-descbinds)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-keymap-prefix (kbd "C-c C-p"))
;; (helm-autoresize-mode 1) ; does not work need to figure out

(install-require 'neotree)
(global-set-key (kbd "C-c k n") 'neotree-toggle) ;; to close need to move out of neo buffer
(setq neo-smart-open t)
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

;; CODE STUFF
(install-require 'flycheck)
(global-flycheck-mode)
;; CODE STUFF - END

;; PYTHON

(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(install-require 'elpy)

(defun my-elpy-enable ()
  (interactive)
  (setq elpy-rpc-backend "jedi")
  (elpy-enable)
  (when (executable-find "ipython")
    (elpy-use-ipython))
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  (setq elpy-modules '(elpy-module-sane-defaults
                       elpy-module-company
                       elpy-module-eldoc
                       elpy-module-highlight-indentation
                       elpy-module-pyvenv
                       elpy-module-yasnippet)))

;;(install-require 'virtualenvwrapper)
;;(install-require 'company)
;;(install-require 'company-jedi)

;;(add-hook 'python-mode-hook '(lambda () (setq python-indent 4)))
;; (install-require 'py-autopep8) ;;(require 'py-autopep8)
;; (add-hook 'before-save-hook 'py-autopep8-before-save)
;;(require 'virtualenvwrapper)
;;(install-require 'nose)
;;(install-require 'jedi)
;;(add-hook 'python-mode-hook 'jedi:setup)
;;(jedi:install-server)
;; (add-hook 'python-mode-hook 'elpy-mode)
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

;; JAVA
(install-require 'emacs-eclim)
(require 'eclimd)
(global-eclim-mode)
(custom-set-variables
 '(eclim-eclipse-dirs '("~/eclipse/active"))
 '(eclim-executable "~/eclipse/active/eclim"))

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(install-require 'auto-complete)
(ac-config-default)
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)
;; JAVA - END


;; Markdown
(install-require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; Markdown
