(defun latex-mode-setup ()
  "Setup latex-mode with packages from the elpa directory"
  (let ((elpa-dir (or modules-elpa-dir
                      (error "latex-mode requires modules-elpa-dir to be set before including"))))

    (defun activate-latex ()
      (my-use-package auctex
        :elpa-dir elpa-dir)
      (add-hook 'LaTeX-mode-hook 'visual-line-mode)
      (add-hook 'LaTeX-mode-hook 'flyspell-mode)
      (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
      (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
      (setq reftex-plug-into-AUCTeX t))

    ;; Note: activate-latex is commented out in original, keeping same behavior
    ;;(activate-latex)
    ))

;; Execute the setup (jQuery-style IIFE pattern)
(latex-mode-setup)