;; core.el --- Core Emacs Configuration
;; 
;;; Commentary:
;; This file contains the essential Emacs configuration that should be loaded
;; for every session. It is designed to be a stable base, providing a
;; consistent experience regardless of the project or context. Functionality
;; that is project-specific is loaded from separate modules.
;; 
;;; Code:

;;; -----------------------------------------------------------------
;;; SECTION: Writable Directories
;;;
;;; WHY-CORE: When config files live in a read-only location (e.g. nix store),
;;; Emacs needs writable paths for packages, auto-saves, and customizations.
;;; -----------------------------------------------------------------
(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))
(make-directory (expand-file-name "auto-save/" user-emacs-directory) t)

;;; -----------------------------------------------------------------
;;; SECTION: ELPA Package Management
;;;
;;; WHY-CORE: This is the foundation of the Emacs configuration. It sets up
;;; the package manager (`package.el`) and defines a helper functions to
;;; use the package by either installing to default location or to a defined directory
;;; -----------------------------------------------------------------
(require 'package)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(defun install (p &optional elpa-dir)
  "Install package p to default elpa directory if the optional elpa_dir is not given or nil
   Install package p to elpa_dir is given, by temporarily reconfiguring
   Emacs's package system to use the specific elpa-dir"
  (if elpa-dir
      (progn
        ;; Ensure the elpa-dir exists
        (unless (file-exists-p elpa-dir)
          (make-directory elpa-dir t))

        ;; Save current state of package system before modification
        (let ((original-user-dir package-user-dir)
              (original-directory-list package-directory-list)
              (original-load-path load-path))

          ;; Temporarily override package directories for local install
          (setq package-user-dir elpa-dir)
          (setq package-directory-list (list elpa-dir))
          ;; Add elpa-dir to load-path (don't replace it)
          (add-to-list 'load-path elpa-dir)

          ;; Install package if not already installed in local elpa-dir
          ;; We need to check within the local context since package-installed-p uses current package-user-dir
          (package-initialize)
          (unless (package-installed-p p)
            (unless package-archive-contents
              (package-refresh-contents))
            (package-install p))

          ;; Restore original package directories and load-path
          (setq package-user-dir original-user-dir)
          (setq package-directory-list original-directory-list)
          (setq load-path original-load-path)
          (package-initialize)))
    ;; Default case: install normally
    (unless (package-installed-p p)
      (package-initialize)
      (unless package-archive-contents
        (package-refresh-contents))
      (package-install p))))

(defun install-require (p)
  (install p)
  (require p))

(install-require 'use-package) ; Ensure use-package is available before defining macros
(install-require 'cl-lib) ; Needed for cl-position and cl-subseq

(defmacro my-use-package (package &rest args)
  "Wrapper around use-package that handles local ELPA directories.
If :elpa-dir is provided, installs package there and uses :load-path.
Otherwise uses :ensure t for standard installation.
In both cases, :demand t is set to ensure the package is loaded."
  (let ((elpa-dir-pos (cl-position :elpa-dir args))
        (filtered-args (copy-sequence args))
        elpa-dir)

    ;; Extract and remove :elpa-dir from args if present
    (when elpa-dir-pos
      (setq elpa-dir (nth (1+ elpa-dir-pos) args))
      ;; Remove :elpa-dir and its value from the args list
      (setq filtered-args (append (cl-subseq args 0 elpa-dir-pos)
                                  (cl-subseq args (+ elpa-dir-pos 2)))))

    (if elpa-dir
        ;; Local ELPA directory case: install to local dir and add to load-path
        `(progn
           (install ',package ,elpa-dir)
           ;; Add all subdirectories of elpa-dir to load-path for package access
           (let ((package-dirs (directory-files ,elpa-dir t "^[^.]")))
             (dolist (dir package-dirs)
               (when (file-directory-p dir)
                 (add-to-list 'load-path dir))))
           (use-package ,package
             :demand t
             ,@filtered-args))
      ;; Standard case: use :ensure t
      `(use-package ,package
         :ensure t
         :demand t
         ,@filtered-args))))

;;; -----------------------------------------------------------------
;;; SECTION: Base UI and Interaction
;;; 
;;; WHY-CORE: This section configures the fundamental look, feel, and
;;; interaction of the Emacs interface. It includes key bindings, mouse
;;; behavior, and visual elements like the theme and line numbers. This
;;; ensures a consistent and predictable user experience in every session.
;;; -----------------------------------------------------------------
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

(pixel-scroll-precision-mode -1) ; Explicitly disable to resolve potential conflicts

(defun activate-colors-visuals ()
  (setq inhibit-startup-screen t) ;; no startup or splash
  (my-use-package zenburn-theme
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


;;; -----------------------------------------------------------------
;;; SECTION: General Editing Enhancements
;;; 
;;; WHY-CORE: This section provides general-purpose editing features that
;;; improve productivity across all file types and projects. This includes
;;; clipboard integration and a file tree navigator.
;;; -----------------------------------------------------------------
(defun activate-general-edit ()
  ;; Some common bindings, not rebinded
  ;; M-; comment dwim
  ;; c-M-a, c-M-e navigate to start and end of function
  ;; c-M-u, c-M-d navigate up or down the stack/list
  ;; M-| pbcopy RET to copy to buffer.
  (my-use-package pbcopy
    :config
    (turn-on-pbcopy))
  (my-use-package neotree
    :bind (("C-c N" . neotree-toggle))
    :config
    (setq neo-smart-open t))
  (setq global-auto-revert-model t)
  (global-visual-line-mode 1)
)
(activate-general-edit)
    

;;; -----------------------------------------------------------------
;;; SECTION: Shell Environment Integration
;;; 
;;; WHY-CORE: Ensures that Emacs inherits the correct PATH and other
;;; environment variables from the user's shell configuration. This is crucial
;;; for external commands and tools to function correctly from within Emacs.
;;; -----------------------------------------------------------------
(defun activate-shell-path-from-shell ()
  (my-use-package exec-path-from-shell
    :config
    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize)))
  (setq shell-command-switch "-ic"))
(activate-shell-path-from-shell)


;;; -----------------------------------------------------------------
;;; SECTION: Backup Configuration
;;; 
;;; WHY-CORE: File safety is paramount. This configures Emacs's backup
;;; system to be robust, creating versioned backups in a centralized
;;; directory. This is a critical, non-negotiable feature for any session.
;;; -----------------------------------------------------------------
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
    ;; Make a "per save" backup on each save.
    (let ((buffer-backed-up nil))
      (backup-buffer)))
  (add-hook 'before-save-hook  'force-backup-of-buffer)

  ;; save tramp to local
  (my-use-package tramp
    :config
    (setq tramp-auto-save-directory "~/.emacs.d/backup/tramp-autosave")))
(activate-backup-settings)


;;; -----------------------------------------------------------------
;;; SECTION: Minibuffer Completion
;;; 
;;; WHY-CORE: Efficient minibuffer completion is one of the most significant
;;; productivity enhancers in Emacs. This section configures the modern
;;; Vertico stack (Vertico, Orderless, Consult, Marginalia, Embark) to provide
;;; a fast, powerful, and intuitive completion experience for all commands.
;;; 
;;; ALTERNATES: Alternative completion frameworks (Ivy, Helm) are available
;;; in `core-alternates-backup.el`.
;;; -----------------------------------------------------------------
(defun activate-vertico ()
  "Modern completion stack: Vertico + Orderless + Consult + Marginalia"

  ;; Vertico - vertical completion UI
  (my-use-package vertico
    :init
    (vertico-mode 1)
    (vertico-mouse-mode 1)
    :custom
    (vertico-count 20)
    (vertico-resize t))

  ;; Orderless - flexible completion matching
  (my-use-package orderless
    :custom
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles partial-completion)))))

  ;; Marginalia - rich annotations
  (my-use-package marginalia
    :init (marginalia-mode 1)
    :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle)))

  ;; Consult - enhanced commands
  (my-use-package consult
    :bind (
      ("C-x b" . consult-buffer)
      ("C-x 4 b" . consult-buffer-other-window)
      ("C-x r b" . consult-bookmark)
      ("C-c f" . consult-fd)
      ("C-s" . consult-line)
      ("C-c s" . consult-line-multi)
      ("C-c g" . consult-ripgrep)
      ("C-c G" . consult-grep)
      ("M-g o" . consult-outline)
      ("M-g i" . consult-imenu)
      ("M-g l" . consult-goto-line)
      ("M-y" . consult-yank-pop)
      ("C-c h" . consult-history)
      )
    :config
    (consult-customize
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file
     :preview-key '(:debounce 0.4 any))
    (when (executable-find "fd")
      (setq consult-find-args "fd --color=never --full-path")))

  ;; Embark - contextual actions
  (my-use-package embark
    :bind (("C-c a" . embark-act)
           ("C-c d" . embark-dwim)
           ("C-c B" . embark-bindings))
    :init (setq prefix-help-command #'embark-prefix-help-command)
    :config
    (add-to-list 'display-buffer-alist
                 '("\`\*Embark Collect \(Live\|Completions\)\*"
                   nil
                   (window-parameters (mode-line-format . none)))))

  ;; Embark-Consult - integration
  (my-use-package embark-consult
    :hook (embark-collect-mode . consult-preview-at-point-mode))
)
(activate-vertico)


;;; -----------------------------------------------------------------
;;; SECTION: General Programming Support
;;; 
;;; WHY-CORE: Provides a baseline of essential tools for software
;;; development that are language-agnostic. This includes on-the-fly syntax
;;; checking (flycheck), code completion (company), snippets (yasnippet),
;;; and improved indentation visualization.
;;; -----------------------------------------------------------------
(defun activate-general-code ()
  ;; electric indent is on by default
  (my-use-package flycheck)
  (my-use-package company
    :custom
      (company-idle-delay 0.25))
  (my-use-package yasnippet
    :config
      (yas-reload-all)
      (add-hook 'prog-mode-hook 'yas-minor-mode)
      (add-hook 'text-mode-hook 'yas-minor-mode))
  (my-use-package highlight-indent-guides
    :custom
      (highlight-indent-guides-method 'character)
      (highlight-indent-guides-responsive 'stack)
    :init
      (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))
)
(activate-general-code)


;;; -----------------------------------------------------------------
;;; SECTION: Git Integration (Lazygit + Magit)
;;;
;;; WHY-CORE: Enhanced git workflow with side-by-side diffs and
;;; seamless magit integration. Loaded from separate core-gittree.el module.
;;; -----------------------------------------------------------------
(let ((gittree-path (concat (file-name-directory (or load-file-name buffer-file-name)) "core-gittree.el")))
  (load-file gittree-path))


;;; -----------------------------------------------------------------
;;; SECTION: Project-Specific Module Loader
;;;
;;; WHY-CORE: This block enables project-specific modularity. It checks
;;; for an environment variable (`EMACS_MODULES`) and, if it's set,
;;; loads the corresponding module files. This allows `direnv` or other
;;; tools to dynamically add functionality to Emacs on a per-project basis.
;;;
;;; NOTE ON PATHS: The loader assumes that all modules are located in the
;;; same directory as this `core.el` file. This is achieved by constructing
;;; the path relative to `load-file-name`, which makes the entire `emacs`
;;; configuration portable.
;;; ----------------------------------------------------------------
(defvar modules-elpa-dir nil
  "Directory for module-specific ELPA packages. Set by activate-modules before loading each module.")

(defun activate-modules ()
  (let ((modules (getenv "EMACS_MODULES"))
        (env-modules-elpa-dir (getenv "EMACS_MODULES_ELPA_DIR")))
    (when modules
      (dolist (module (split-string modules))
        (let ((module_path (concat
                            (file-name-directory (or load-file-name buffer-file-name))
                            module ".el")))
          (if (file-exists-p module_path)
              (progn
                ;; Set the elisp variable for the module to use
                (setq modules-elpa-dir env-modules-elpa-dir)
                (load-file module_path))
            (message "Emacs module not found: %s" module_path))))))
)
(activate-modules)

;; Load all the custom thingies that other modules would have written
(when (file-exists-p custom-file) (load custom-file))

(provide 'core)
;;; core.el ends here
