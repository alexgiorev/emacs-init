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
(add-hook
 'python-mode-hook
 (lambda ()
   (define-key python-mode-map (kbd "M-{") 'python-nav-backward-block)
   (define-key python-mode-map (kbd "M-}") 'python-nav-forward-block)
   (define-key python-mode-map (kbd "M-h") 'python-mark-defun)))

;; useful for copying functions into Anki
(defun my/python-kill-for-anki (beg end)
  "This addresses the problem taht indented functions come out funny in Anki's
code generation tool"
  (interactive "r")
  (let ((func (buffer-substring beg end))
        indent)
    (with-temp-buffer
      (insert func)
      (goto-char (point-min))
      (setq indent (current-indentation))
      (my/maplines (point-min) (point-max)
                   (lambda () (my/reduce-indentation indent)))
      (kill-region (point-min) (point-max)))))

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c M-w")
    'my/python-kill-for-anki))
  
