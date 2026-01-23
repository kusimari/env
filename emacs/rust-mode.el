(defun rust-mode-setup ()
  "Setup rust-mode with packages from the elpa directory set by core.el"
  (let ((elpa-dir (or modules-elpa-dir
                      (error "rust-mode requires modules-elpa-dir to be set before including"))))

    (defun activate-rust-2 ()
      ;; Basic rust-mode for syntax highlighting
      (my-use-package rust-mode
        :elpa-dir elpa-dir
        :config
        (setq rust-format-on-save t))
      ;; Advanced Rust support
      (my-use-package rustic
        :elpa-dir elpa-dir)
      (my-use-package lsp-mode
        :elpa-dir elpa-dir
        :commands lsp
        :custom
          (lsp-inlay-hint-enable t)
        :config
          (add-hook 'lsp-mode-hook 'lsp-ui-mode))
      (my-use-package lsp-ui
        :elpa-dir elpa-dir
        :commands lsp-ui-mode
        :custom
        (lsp-ui-peek-always-show t)
        (lsp-ui-sideline-show-hover t)
        (lsp-ui-doc-enable nil)))

    ;; Activate rust configuration
    (activate-rust-2)))

;; Execute the setup (jQuery-style IIFE pattern)
(rust-mode-setup)