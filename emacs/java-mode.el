(defun java-mode-setup ()
  "Setup java-mode with packages from the elpa directory"
  (let ((elpa-dir (or modules-elpa-dir
                      (error "java-mode requires modules-elpa-dir to be set before including"))))

    (defun activate-java ()
      (my-use-package emacs-eclim
        :elpa-dir elpa-dir)
      (require 'eclimd)
      (global-eclim-mode)
      (custom-set-variables
       '(eclim-eclipse-dirs '("~/eclipse/active"))
       '(eclim-executable "~/eclipse/active/eclim"))

      (setq help-at-pt-display-when-idle t)
      (setq help-at-pt-timer-delay 0.1)
      (help-at-pt-set-timer)

      (my-use-package auto-complete
        :elpa-dir elpa-dir)
      (ac-config-default)
      (require 'ac-emacs-eclim-source)
      (ac-emacs-eclim-config))

    ;; Note: activate-java is commented out in original, keeping same behavior
    ;;(activate-java)
    ))

;; Execute the setup (jQuery-style IIFE pattern)
(java-mode-setup)