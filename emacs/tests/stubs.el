;;; stubs.el --- Minimal stubs so core-gittree.el loads in batch -*- lexical-binding: t -*-

;;; Commentary:
;; core-gittree.el is normally loaded by core.el and depends on the
;; `my-use-package' macro plus treemacs/vdiff/ediff symbols defined
;; therein. This file provides enough stubs to load core-gittree.el in
;; `emacs --batch' without MELPA.
;;
;; Load order: stubs.el -> core-gittree.el -> helpers.el -> test-*.el

;;; Code:

(require 'cl-lib)

;; my-use-package: in the real config this wraps use-package with a
;; local-ELPA path. In batch we just ignore the whole form — the only
;; thing we need is for the macro expansion not to error at load-time.
(defmacro my-use-package (_package &rest _args) nil)

;; install / install-require: used elsewhere in core.el; stub to be safe.
(defun install (&rest _) nil)
(defun install-require (&rest _) nil)

;; Symbols core-gittree.el references at top level / in stubs.
(defvar treemacs-file-name-transformer nil)
(defvar treemacs-default-visit-action nil)
(defvar treemacs-mode-map (make-sparse-keymap))
(defvar vdiff-mode nil)

;; No-op functions for any shapes referenced by core-gittree.el outside
;; of gittree-launch's direct buffer-creation path.
(dolist (sym '(treemacs-get-local-window
               treemacs-add-project-to-workspace
               treemacs
               treemacs-quit
               treemacs--do-refresh
               treemacs-run-in-every-buffer
               treemacs--prop-at-point
               vdiff-buffers
               vdiff-quit
               vdiff--diff-refresh-finish
               ediff-buffers))
  (unless (fboundp sym)
    (fset sym (lambda (&rest _) nil))))

(provide 'stubs)
;;; stubs.el ends here
