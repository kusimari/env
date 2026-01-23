(defun nix-mode-setup ()
  "Setup nix-mode with packages from the elpa directory"
  (let ((elpa-dir (or modules-elpa-dir
                      (error "nix-mode requires modules-elpa-dir to be set before including"))))

    (defun activate-nix ()
      (my-use-package nix-mode
        :elpa-dir elpa-dir
        :mode ("\\.nix\\'" )
        :config
        (setq nix-indent-function 'smie-indent-line)))

    ;; Activate nix configuration
    (activate-nix)))

;; Execute the setup (jQuery-style IIFE pattern)
(nix-mode-setup)