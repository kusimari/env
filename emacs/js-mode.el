(defun js-mode-setup ()
  "Setup js-mode with packages from the elpa directory"
  (let ((elpa-dir (or modules-elpa-dir
                      (error "js-mode requires modules-elpa-dir to be set before including"))))

    (defun activate-js ()
      (my-use-package js2-mode
        :elpa-dir elpa-dir)
      (my-use-package tern
        :elpa-dir elpa-dir)
      (my-use-package company-tern
        :elpa-dir elpa-dir)

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

    ;; Note: activate-js is commented out in original, keeping same behavior
    ;;(activate-js)
    ))

;; Execute the setup (jQuery-style IIFE pattern)
(js-mode-setup)