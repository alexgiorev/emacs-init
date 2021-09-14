;; useful for copying functions into Anki
(defun my/kill-code-for-anki (beg end)
  "This addresses the problem of indented functions coming out funny in Anki's
code generation tool"
  (interactive "r")
  (save-excursion
    (goto-char beg) (beginning-of-line) (setq beg (point))  
    (let ((func (buffer-substring beg end))
          indent)
      (with-temp-buffer
        (insert func)
        (my/touch-left (point-min) (point-max))
        (kill-region (point-min) (point-max))))))

(with-eval-after-load 'prog-mode
  (define-key prog-mode-map
    (kbd "C-c M-w") 'my/kill-code-for-anki))
