;;════════════════════════════════════════════════════════════
;; misc

;; useful for copying functions into Anki
(defun my-save-code-remove-indent (beg end)
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
    (kbd "C-c M-w") 'my-save-code-remove-indent))

(defun my-insert-ruler (length)
  (interactive "p")
  (insert (make-string length ?═)))
(define-key prog-mode-map (kbd "C-x C-r") 'my-insert-ruler)

;; ########################################
(require 'cpath)
;; ########################################
(provide 'my-code)
