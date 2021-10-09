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

;; ----------------------------------------
;; 
