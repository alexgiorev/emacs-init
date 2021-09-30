;; useful for copying functions into Anki
(defun my-kill-code-remove-indent (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg) (beginning-of-line) (setq beg (point))  
    (let ((code (buffer-substring beg end)))
      (with-temp-buffer
        (insert code)
        (untabify (point-min) (point-max))
        (my-touch-left (point-min) (point-max))
        (kill-region (point-min) (point-max)))
      (deactivate-mark))))

(with-eval-after-load 'prog-mode
  (define-key prog-mode-map
    (kbd "C-c M-w") 'my-kill-code-remove-indent))

;; ----------------------------------------
;; magit

(defun my-magit-commit-file (&optional ammend)
  "Stage the current file and commit. For convenience, attempts to put in the
kill ring the name of the defun at point."
  (interactive)
  (ignore-errors
    (kill-new (my-get-defun-name)))
  (magit-stage-file (buffer-file-name))
  (if ammend (magit-commit-amend) (magit-commit)))

(defvar my-magit-map (make-sparse-keymap))
(progn
  (define-key my-magit-map "c" 'my-magit-commit-file)
  (define-key my-magit-map "a"
    (lambda nil (interactive) (my-magit-commit-file :amend)))
  (define-key my-magit-map "p" 'magit-push-current-to-upstream))
(define-key prog-mode-map "\C-cg" my-magit-map)
