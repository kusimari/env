;;; test-gittree.el --- Batch tests for core-gittree.el -*- lexical-binding: t -*-

;;; Commentary:
;; Exercises gittree-launch + gittree--create-git-buffer +
;; gittree--get-ref-diff-status against a fixture repo built by
;; helpers.el. Run via `emacs --batch -l helpers.el -l test-gittree.el
;; -f gittree-run-tests`.

;;; Code:

(require 'helpers)

(gittree-test--install-stubs)

(defun test-gittree--buffer-for (file-path ref)
  "Return the buffer `gittree--create-buffer' would produce, in
`default-directory'. Kills any existing buffer with the same name so
each test starts clean (buffers are read-only once created)."
  (let* ((expanded (expand-file-name file-path))
         (file-name (file-name-nondirectory expanded))
         (ref-display (cond ((string= ref "HEAD") "HEAD")
                            ((string= ref ":0") "staged")
                            ((string= ref "working") "working")
                            ((string= ref "empty") "empty")
                            (t ref)))
         (existing (get-buffer (format "*%s@%s*" file-name ref-display))))
    (when existing (let ((kill-buffer-query-functions nil)) (kill-buffer existing)))
    ;; Also nuke any "(not in ...)" variant from a prior run
    (dolist (b (buffer-list))
      (let ((n (buffer-name b)))
        (when (and n (string-match-p (regexp-quote file-name) n)
                   (string-match-p "not in" n))
          (let ((kill-buffer-query-functions nil)) (kill-buffer b))))))
  (let ((buf (gittree--create-buffer (expand-file-name file-path) ref)))
    (gittree-test-assert (bufferp buf)
                         (format "expected a buffer for %s@%s, got %S"
                                 file-path ref buf))
    buf))

(defun test-gittree--buffer-string (buf)
  (with-current-buffer buf (buffer-string)))

(gittree-test-deftest tc1-zero-arg-mode-toggle
  ;; `gittree-mode' should be callable in batch (stubs make enable/disable
  ;; a no-op at the treemacs layer) without erroring.
  (gittree-mode 1)
  (gittree-test-assert gittree-mode "gittree-mode should be non-nil after (gittree-mode 1)")
  (gittree-mode -1)
  (gittree-test-assert (not gittree-mode) "gittree-mode should be nil after (gittree-mode -1)"))

(gittree-test-deftest tc2-added-file-empty-left
  ;; gamma.md was added in commit2. Left side at HEAD~1 must be empty
  ;; (not a git error), right side must have the added content.
  (let* ((left (test-gittree--buffer-for "gamma.md" "HEAD~1"))
         (right (test-gittree--buffer-for "gamma.md" "HEAD"))
         (left-s (test-gittree--buffer-string left))
         (right-s (test-gittree--buffer-string right)))
    (gittree-test-assert-equal left-s ""
      "Left buffer for gamma.md@HEAD~1 must be empty (file didn't exist then)")
    (gittree-test-assert-match "gamma new file" right-s
      "Right buffer for gamma.md@HEAD must contain the added content")
    (gittree-test-assert-no-match "fatal:\\|exists on disk" left-s
      "Left buffer must not contain raw git error text")
    (gittree-test-assert-no-match "fatal:\\|exists on disk" right-s
      "Right buffer must not contain raw git error text")))

(gittree-test-deftest tc3-deleted-file-empty-right
  ;; beta.md was deleted in commit2. Left has old content, right is empty.
  (let* ((left (test-gittree--buffer-for "beta.md" "HEAD~1"))
         (right (test-gittree--buffer-for "beta.md" "HEAD"))
         (left-s (test-gittree--buffer-string left))
         (right-s (test-gittree--buffer-string right)))
    (gittree-test-assert-match "beta content" left-s
      "Left buffer for beta.md@HEAD~1 must contain the original content")
    (gittree-test-assert-equal right-s ""
      "Right buffer for beta.md@HEAD must be empty (file was deleted)")
    (gittree-test-assert-no-match "fatal:\\|exists on disk" left-s
      "Left buffer must not contain raw git error text")
    (gittree-test-assert-no-match "fatal:\\|exists on disk" right-s
      "Right buffer must not contain raw git error text")))

(gittree-test-deftest tc4-modified-file-both-populated
  ;; alpha.md is modified between commits. Both sides populated, contents differ.
  (let* ((left (test-gittree--buffer-for "alpha.md" "HEAD~1"))
         (right (test-gittree--buffer-for "alpha.md" "HEAD"))
         (left-s (test-gittree--buffer-string left))
         (right-s (test-gittree--buffer-string right)))
    (gittree-test-assert-match "alpha v1" left-s
      "Left buffer for alpha.md@HEAD~1 must contain v1 content")
    (gittree-test-assert-match "alpha v2 modified" right-s
      "Right buffer for alpha.md@HEAD must contain v2 content")
    (gittree-test-assert (not (equal left-s right-s))
      "Left and right buffers must differ for a modified file")))

(gittree-test-deftest tc5-ref-diff-status
  ;; gittree--get-ref-diff-status should report A/M/D for the three paths.
  (let* ((alist (gittree--get-ref-diff-status "HEAD~1" "HEAD"))
         (by-name (mapcar (lambda (cell)
                            (cons (file-name-nondirectory (car cell)) (cdr cell)))
                          alist))
         (status-for (lambda (name) (cdr (assoc name by-name)))))
    (gittree-test-assert-match "M" (or (funcall status-for "alpha.md") "")
      "alpha.md should appear as modified")
    (gittree-test-assert-match "D" (or (funcall status-for "beta.md") "")
      "beta.md should appear as deleted")
    (gittree-test-assert-match "A" (or (funcall status-for "gamma.md") "")
      "gamma.md should appear as added")
    ;; A file modified-between-commits that the working tree hasn't changed
    ;; should still appear here.
    (gittree-test-assert (funcall status-for "alpha.md")
      "alpha.md must be in the ref-diff status alist")))

(gittree-test-deftest tc6-short-sha
  ;; Same as TC4 but with a short SHA rather than HEAD~1.
  (let* ((sha1 (or (getenv "GITTREE_TEST_SHA1")
                   (error "GITTREE_TEST_SHA1 env var must be set by the runner")))
         (short (substring sha1 0 7))
         (left (test-gittree--buffer-for "alpha.md" short))
         (right (test-gittree--buffer-for "alpha.md" "HEAD"))
         (left-s (test-gittree--buffer-string left))
         (right-s (test-gittree--buffer-string right)))
    (gittree-test-assert-match "alpha v1" left-s
      (format "alpha.md@%s must contain v1 content" short))
    (gittree-test-assert-match "alpha v2 modified" right-s
      "alpha.md@HEAD must contain v2 content")
    (gittree-test-assert-no-match "fatal:" left-s
      "No git error text in left buffer")))

(provide 'test-gittree)
;;; test-gittree.el ends here
