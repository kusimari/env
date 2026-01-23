(defun python-mode-setup ()
  "Setup python-mode with packages from the elpa directory"
  (let ((elpa-dir (or modules-elpa-dir
                      (error "python-mode requires modules-elpa-dir to be set before including"))))

    (defun activate-python ()
      (my-use-package lsp-jedi
        :elpa-dir elpa-dir)
      ;; (my-use-package elpy
      ;;   :elpa-dir elpa-dir
      ;;   :init
      ;;   (setq elpy-rpc-virtualenv-path 'current)
      ;;   (elpy-enable))
      ;; (my-use-package company-jedi
      ;;   :elpa-dir elpa-dir)
      ;; (defun my/python-mode-hook ()
      ;; (add-to-list 'company-backends 'company-jedi))
      ;; (add-hook 'python-mode-hook 'my/python-mode-hook)
      )

    ;; Activate python configuration
    (activate-python)))

;; Execute the setup (jQuery-style IIFE pattern)
(python-mode-setup)