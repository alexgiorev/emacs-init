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
(defconst my-elisp-defun-re
  "([[:space:]\n]*defun[[:space:]\n]+\\(\\(?:\\sw\\|\\s_\\)+\\)")

(defun my-elisp-isearch-defun nil
  (interactive)
  (let ((isearch-filter-predicate
         (lambda (beg end)
           (save-match-data
             (save-excursion
               (beginning-of-line)
               (and (looking-at my-elisp-defun-re)
                    (<= (match-beginning 1) beg end (match-end 1))))))))
    (isearch-mode :forward nil nil t)))

(define-key emacs-lisp-mode-map (kbd "M-s d") 'my-elisp-isearch-defun)
