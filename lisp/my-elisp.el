(require 'elisp-mode)
;; ########################################
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

(defun my-elisp-defun-name nil
  (save-excursion
    (beginning-of-line)
    (unless (looking-at my-elisp-defun-re)
      (beginning-of-defun))
    (let (beg end)
      (forward-symbol 2) (setq end (point))
      (forward-symbol -1) (setq start (point))
      (buffer-substring-no-properties start end))))

(defun my-elisp-save-defun-name nil
  "Puts in the kill ring the name of the current defun. Useful when taking notes
about the function."
  (interactive)
  (kill-new (my-elisp-defun-name)))

(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-t")
    'my-elisp-save-defun-name))

;; ########################################
(define-key emacs-lisp-mode-map (kbd "C-c t")
  (lambda nil
    (interactive)
    (insert "(error \"TODO\")")))

;; ########################################
;; cpath
(with-eval-after-load 'cpath
  (add-hook 'emacs-lisp-mode-hook
            (lambda nil
              (setq cpath-func-name-func
                    'my-elisp-defun-name))))
;;########################################
;; evaluation

(defun my-elisp-eval-defvar nil
  "When the expression before point is a defvar, evaluate the expression and set
it as the symbol's value"
  (interactive)
  (let ((sexp (elisp--preceding-sexp))
        var body)
    (when (eq (car-safe sexp) 'defvar)
      (setq var (cadr sexp) body (caddr sexp))
      (eval `(setq ,var ,body))
      (message (symbol-name var)))))

(defvar my-elisp-eval-map (make-sparse-keymap))
(progn
  (define-key my-elisp-eval-map "\C-e" 'eval-last-sexp)
  (define-key my-elisp-eval-map "\C-b" 'eval-buffer)
  (define-key my-elisp-eval-map "\C-r" 'eval-region)
  (define-key my-elisp-eval-map "\C-v" 'my-elisp-eval-defvar))
(define-key emacs-lisp-mode-map (kbd "C-x C-e") my-elisp-eval-map)

;; ########################################
(provide 'my-elisp)