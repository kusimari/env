(defun haskell-mode-setup ()
  "Setup haskell-mode with packages from the elpa directory"
  (let ((elpa-dir (or modules-elpa-dir
                      (error "haskell-mode requires modules-elpa-dir to be set before including"))))

    (defun activate-haskell ()
      (my-use-package haskell-mode
        :elpa-dir elpa-dir)
      (require 'haskell-interactive-mode)
      (require 'haskell-process)
      (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
      ;; (custom-set-variables '(haskell-process-type 'stack-ghci))
      (setq haskell-process-type 'stack-ghci)
      (custom-set-variables '(haskell-process-path-stack "stack"))
      (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
      (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
      (eval-after-load "haskell-mode"
        '(progn
           (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
           (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right))))

    ;; Note: activate-haskell is commented out in original, keeping same behavior
    ;;(activate-haskell)
    ))

;; Execute the setup (jQuery-style IIFE pattern)
(haskell-mode-setup)