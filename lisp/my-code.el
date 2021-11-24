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

(defvar my-ruler-regexp "═+$")
(defun my-next-ruler nil
  (interactive)
  (push-mark)
  (beginning-of-line 2)
  (re-search-forward my-ruler-regexp nil :move)
  (beginning-of-line))

(defun my-prev-ruler nil
  (interactive)
  (push-mark)
  (beginning-of-line 0)
  (re-search-backward my-ruler-regexp nil :move)
  (beginning-of-line))

(require 'cpath)

;;════════════════════════════════════════
(provide 'my-code)
