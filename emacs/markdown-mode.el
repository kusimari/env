(defun markdown-mode-setup ()
  "Setup markdown-mode with packages from the elpa directory"
  (let ((elpa-dir (or modules-elpa-dir
                      (error "markdown-mode requires modules-elpa-dir to be set before including"))))

    (defun activate-markdown ()
      (my-use-package markdown-mode
        :elpa-dir elpa-dir)
      (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

    ;; Note: activate-markdown is commented out in original, keeping same behavior
    ;;(activate-markdown)
    ))

;; Execute the setup (jQuery-style IIFE pattern)
(markdown-mode-setup)