;; ELPA PACKAGES
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

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

(install-require 'use-package)
;; ELPA PACKAGES


(defun activate-keys ()
  ;; (setq x-alt-keysym 'meta) ;; map iterm2 or alacritty keys to send alt on esc
)
(activate-keys)


(defun activate-mouse-scroll ()
  (defun my-bell-function ()
    (unless (memq this-command
                  '(scroll-up-line scroll-down-line))
      (ding)))
  (setq ring-bell-function 'my-bell-function)

  (xterm-mouse-mode t)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))
(activate-mouse-scroll)


(defun activate-colors-visuals ()
  (setq inhibit-startup-screen t) ;; no startup or splash
  (use-package zenburn-theme
    :ensure t
    :config
    (load-theme 'zenburn t))
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

  (global-display-line-numbers-mode t)
  (setq column-number-mode t)
)
(activate-colors-visuals)


(defun activate-general-edit ()
  ;; Some common bindings, not rebinded
  ;; M-; comment dwim
  ;; c-M-a, c-M-e navigate to start and end of function
  ;; c-M-u, c-M-d navigate up or down the stack/list
  ;; M-| pbcopy RET to copy to buffer. For tmux https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard.. and use the right shell
  (install-require 'pbcopy)
  (turn-on-pbcopy)
  (use-package neotree
    :ensure t
    :bind (("C-c N" . neotree-toggle))
    :config
    (setq neo-smart-open t))
  (setq global-auto-revert-model t)
)
(activate-general-edit)
    

(defun activate-shell-path-from-shell ()
  (install-require 'exec-path-from-shell)
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
  (setq shell-command-switch "-ic"))
(activate-shell-path-from-shell)


(defun activate-backup-settings ()
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
  (install-require 'tramp)
  (setq tramp-auto-save-directory "~/.emacs.d/backup/tramp-autosave"))
(activate-backup-settings)


;; minibuffer completion setup - ivy, helm
(defun activate-ivy ()
  (use-package ivy  ;; replace emacs default ido completion
    :ensure t
    :config
       (setq ivy-use-virtual-buffers t)
       (setq ivy-count-format "(%d/%d) ")
       (setq ivy-initial-inputs-alist '())
       (ivy-mode 1)
       (counsel-mode 1))
  (use-package counsel ;; bind keys to ivy/counsel versions
    :ensure t
    :bind (("M-x" . counsel-M-x)
           ("C-x C-f" . counsel-find-file)
           ("C-x b" . ivy-switch-buffer)
           ("M-y" . counsel-yank-pop)))
  (use-package swiper  ;; better search completions
    :ensure t
    :bind (("C-s" . swiper-isearch)))  ;; M-q for replace search match
  (use-package counsel-projectile
    ;; projectile lets you do all ivy/counser/swiper within
    ;; the constraints of a project (usally a git folder or folder with a project toml file, etc)
    :ensure t
    :bind (("C-c p" . projectile-command-map))
    :config (counsel-projectile-mode 1))
  (use-package perspective
    ;; perspectives of a window, buffer, layout within a project and across
    :ensure t
    :bind
      ("C-x C-b" . persp-list-buffers)
    :custom
      (persp-mode-prefix-key (kbd "C-c M-p"))
    :init
      (persp-mode))
)
;;(activate-ivy)

(defun activate-helm ()
  ;;(install-require 'sr-speedbar)
  (install-require 'async)
  (use-package helm
    :ensure t
    :demand t
    :init
    (require 'helm-config)
    (setq helm-command-prefix-key "C-c h")
    :config
    (setq helm-split-window-in-side-p    t
          helm-autoresize-max-height     25
          helm-autoresize-min-height     10)
    (helm-mode 1))
  ;; (use-package projectile :ensure t
  ;;   :init
  ;;   (projectile-mode +1)
  ;;   :bind (:map projectile-mode-map
  ;;               ("s-p" . projectile-command-map)
  ;;               ("C-c C-p" . projectile-command-map)))
  ;; (use-package helm-projectile)
  ;; (install-require 'helm-projectile)
  ;; (install-require 'helm-descbinds)
  ;; (install-require 'helm-ls-git)

  ;; (helm-autoresize-mode 1)
  ;; (helm-mode 1)
  ;; (helm-descbinds-mode)  ;; C-c C-h

  ;; (helm-projectile-on)
  ;; (setq projectile-completion-system 'helm)

)
;;(activate-helm)

;; minibuffer completion setup - modern vertico stack
(defun activate-vertico ()
  "Modern completion stack: Vertico + Orderless + Consult + Marginalia"

  ;; Vertico - vertical completion UI
  (use-package vertico
    :ensure t
    :init
    (vertico-mode 1)
    :custom
    (vertico-count 20)  ;; Show more candidates
    (vertico-resize t)) ;; Resize minibuffer to fit content

  ;; Orderless - flexible completion matching with space-separated patterns
  (use-package orderless
    :ensure t
    :custom
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles partial-completion)))))

  ;; Marginalia - rich annotations in minibuffer
  (use-package marginalia
    :ensure t
    :bind (:map minibuffer-local-map
           ("M-A" . marginalia-cycle))  ;; Cycle through different annotation modes
    :init
    (marginalia-mode 1))

  ;; Consult - enhanced commands
  (use-package consult
    :ensure t
    :bind (
      ("C-x b" . consult-buffer)          ;; Enhanced buffer switching with recent files
      ("C-x 4 b" . consult-buffer-other-window) ;; Buffer in other window
      ("C-x r b" . consult-bookmark)      ;; Browse bookmarks
      ;; find files/project
      ("C-c f" . consult-fd)              ;; Find files with fd (faster)

      ;; search open buffers
      ("C-s" . consult-line)              ;; Replace default search (optional - comment out to keep isearch)
      ("C-c s" . consult-line-multi)      ;; Search across multiple buffers
      ;; search across project
      ("C-c g" . consult-ripgrep)         ;; Search content across project
      ("C-c G" . consult-grep)            ;; Grep search (fallback)

      ;; Navigate by structure
      ("M-g o" . consult-outline)         ;; Jump to headings/outline
      ("M-g i" . consult-imenu)           ;; Jump to functions/classes in current file
      ("M-g l" . consult-goto-line)       ;; Enhanced goto-line

      ;; Enhanced kill ring
      ("M-y" . consult-yank-pop)          ;; Enhanced yank-pop with search

      ;; History
      ("C-c h" . consult-history)         ;; Command history
      )
    :config
    ;; Configure preview for search commands
    (consult-customize
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file
     :preview-key '(:debounce 0.4 any))

    ;; Use fd if available for faster file finding
    (when (executable-find "fd")
      (setq consult-find-args "fd --color=never --full-path")))

  ;; Embark - contextual actions on completion targets (terminal-friendly keybindings)
  (use-package embark
    :ensure t
    :bind (("C-c a" . embark-act)         ;; Pick an action to perform
           ("C-c d" . embark-dwim)        ;; Default action (do what I mean)
           ("C-c B" . embark-bindings))   ;; Show all available actions
    :init
    ;; Optionally replace the key help with a completing-read interface
    (setq prefix-help-command #'embark-prefix-help-command)
    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none)))))

  ;; Embark-Consult - enhanced integration between Embark and Consult
  (use-package embark-consult
    :ensure t
    :hook (embark-collect-mode . consult-preview-at-point-mode))
)
(activate-vertico)


(defun activate-general-code ()
  ;; electric indent is on by default
  (use-package flycheck :ensure)
  (use-package company
    :ensure
    :custom
      (company-idle-delay 0.25))
  (use-package yasnippet
    :ensure
    :config
      (yas-reload-all)
      (add-hook 'prog-mode-hook 'yas-minor-mode)
      (add-hook 'text-mode-hook 'yas-minor-mode))
  (use-package highlight-indent-guides ;;https://github.com/DarthFennec/highlight-indent-guides
    :ensure t
    :custom
      (highlight-indent-guides-method 'character)
      (highlight-indent-guides-responsive 'stack)
      (highlight-indent-guides-auto-stack-odd-face-perc 10)
      (highlight-indent-guides-auto-stack-even-face-perc 20)
      (highlight-indent-guides-auto-stack-character-face-perc 40)
    :init
    (progn
      (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))
    )
)
(activate-general-code)


(defun activate-nix ()
  (use-package nix-mode
    :ensure t
    :mode ("\\.nix\\'")
    :config (setq nix-indent-function 'smie-indent-line))
)
(activate-nix)


(defun activate-rust-1 ()
  (use-package flycheck :ensure t)
  (use-package flycheck-rust :ensure t)
  (use-package company :ensure t)
  (use-package rust-mode :ensure t
    :config
    (setq rust-format-on-save t))
  
  (setq racer-rust-src-path
        (concat (string-trim
                 (shell-command-to-string "rustc --print sysroot"))
                "/lib/rustlib/src/rust/library/"))
  (use-package racer :ensure t
    :requires flycheck-mode
    :requires company-mode
    :requires rust-mode

    :config
    (progn
      (add-hook 'rust-mode-hook #'racer-mode)
      (add-hook 'racer-mode-hook #'eldoc-mode)
      (add-hook 'racer-mode-hook #'company-mode))

  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (local-set-key (kbd "TAB") 'company-indent-or-complete-common)
  (setq-local company-tooltip-align-annotations t))
)

(defun activate-rust-2 ()
  (use-package rustic
    :ensure)
  (use-package lsp-mode
    :ensure
    :commands lsp
    :custom
      (lsp-inlay-hint-enable t)
    :config
      (add-hook 'lsp-mode-hook 'lsp-ui-mode))
  (use-package lsp-ui
    :ensure
    :commands lsp-ui-mode
    :custom
    (lsp-ui-peek-always-show t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-doc-enable nil))
)

(defun activate-rust ()
  (use-package eglot :ensure t)
)
(activate-rust)


(defun activate-python ()
  (use-package lsp-jedi :ensure t)
  ;; (use-package elpy
  ;;  :ensure t
  ;;  :init
  ;;  (setq elpy-rpc-virtualenv-path 'current)
  ;;  (elpy-enable))
  ;; (use-package company-jedi
  ;;   :ensure t)
  ;; (defun my/python-mode-hook ()
  ;; (add-to-list 'company-backends 'company-jedi))
  ;; (add-hook 'python-mode-hook 'my/python-mode-hook)
)
(activate-python)


(defun activate-js ()
  (install-require 'js2-mode)
  (install-require 'tern)
  (install-require 'company-tern)
  (add-to-list 'company-backends 'company-tern)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . js2-mode))
  (add-hook 'js2-mode-hook
            (lambda () (tern-mode t)))
  ;; (eval-after-load 'tern '(tern-ac-setup))

  ;; (setq auto-complete-mode 1)
  ;; (setq tern-ac-on-dot 1)
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

  (setq js2-strict-missing-semi-warning nil)
  ;; (add-to-list 'load-path "/usr/local/lib/node_modules/tern/emacs/")
  ;; (autoload 'tern-mode "tern.el" nil t)
)
;;(activate-js)


(defun activate-haskell ()
  (install-require 'haskell-mode)
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
  ;; (custom-set-variables '(haskell-process-type 'stack-ghci))
  (setq haskell-process-type 'stack-ghci)
  (custom-set-variables '(haskell-process-path-stack "stack"))
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (eval-after-load "haskell-mode"
    '(progn
       (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
       (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)))
;;(activate-haskell)


(defun activate-latex ()
  (install-require 'auctex)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t))
;;(activate-latex)


(defun activate-scala ()
  ;;(install-require 'scala-mode2)
  ;;(install-require 'sbt-mode)
  (install-require 'ensime)
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))
;;(activate-scala)


(defun activate-java ()
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
  (ac-emacs-eclim-config))
;;(activate-java)

(defun activate-markdown ()
  (install-require 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))
;;(activate-markdown)
