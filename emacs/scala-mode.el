(defun scala-mode-setup ()
  "Setup scala-mode with packages from the elpa directory"
  (let ((elpa-dir (or modules-elpa-dir
                      (error "scala-mode requires modules-elpa-dir to be set before including"))))

    (defun activate-scala ()
      ;;(my-use-package scala-mode2
      ;;  :elpa-dir elpa-dir)
      ;;(my-use-package sbt-mode
      ;;  :elpa-dir elpa-dir)
      (my-use-package ensime
        :elpa-dir elpa-dir)
      (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

    ;; Note: activate-scala is commented out in original, keeping same behavior
    ;;(activate-scala)
    ))

;; Execute the setup (jQuery-style IIFE pattern)
(scala-mode-setup)