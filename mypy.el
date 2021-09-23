;; Python-related configuration

;; so that python3 is used rather than python2
(setq python-shell-interpreter "python3")

(add-hook 'python-mode-hook 'visual-line-mode)

;; store "raise NotImplementedError" in register 1 for python-mode
(add-hook 'python-mode-hook
          (lambda ()
            (push (cons ?1 "raise NotImplementedError")
                  register-alist)))

;; python-mode keys
(with-eval-after-load 'python-mode
  (define-key python-mode-map (kbd "M-{") 'python-nav-backward-block)
  (define-key python-mode-map (kbd "M-}") 'python-nav-forward-block)
  (define-key python-mode-map (kbd "M-h") 'python-mark-defun))

;; ----------------------------------------
(defun my-rename-python-init ()
  (interactive)
  (let* ((dirname (car (last (split-string (buffer-file-name) "/") 2)))
         (new-buffer-name (concat dirname "__init__.py")))
    (rename-buffer new-buffer-name :unique)))

(add-hook 'python-mode-hook
          (lambda ()
            (if (string-match "^__init__\\.py" (buffer-name))
                (my-rename-python-init))))

(defun my-python-get-func-signature ()
  "Insert into the kill ring the signature of the function at point"
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (looking-at "[ \t]*\\(.*\\):")
    (kill-new (match-string-no-properties 1))))

(with-eval-after-load 'python-mode
  (define-key python-mode-map (kbd "C-c t") 'my-python-get-func-signature))
