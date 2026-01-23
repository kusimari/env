;;; core-alternates-backup.el --- Alternative configurations for completion frameworks

;;; Commentary:
;; This file contains alternative setups for completion frameworks like Ivy and Helm.
;; These are not loaded by default but are kept here for easy switching or reference.

;;; Code:

;; minibuffer completion setup - ivy
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


;; minibuffer completion setup - helm
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

(provide 'core-alternates-backup)

;;; core-alternates-backup.el ends here
