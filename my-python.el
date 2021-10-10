;; Python-related configuration
;; ----------------------------------------
(require 'python)
;; ----------------------------------------
;; so that python3 is used rather than python2
(setq python-shell-interpreter "python3")

(add-hook 'python-mode-hook 'visual-line-mode)

;; store "raise NotImplementedError" in register 1 for python-mode
(add-hook 'python-mode-hook
          (lambda nil
            (set-register ?1 "raise NotImplementedError")))

;; python-mode keys
(define-key python-mode-map (kbd "M-{") 'python-nav-backward-block)
(define-key python-mode-map (kbd "M-}") 'python-nav-forward-block)
(define-key python-mode-map (kbd "M-h") 'python-mark-defun)

;; ----------------------------------------
(defun my-rename-python-init nil
  (interactive)
  (let* ((dirname (car (last (split-string (buffer-file-name) "/") 2)))
         (new-buffer-name (concat dirname "__init__.py")))
    (rename-buffer new-buffer-name :unique)))

(add-hook 'python-mode-hook
          (lambda nil
            (if (string-match "^__init__\\.py" (buffer-name))
                (my-rename-python-init))))

;; ----------------------------------------
;; functions and classes

(defconst my-python-identifier-re "[0-9a-zA-Z_]+")
(defconst my-python-def-re
  (format "^[ \t]*def[ \t]\\(%s\\)" my-python-identifier-re)
  "A regexp which matches the beginning of a function definition. The name of
  the function is stored in the first group.")
(defconst my-python-class-re
  (format "^[ \t]*class[ \t]\\(%s\\)" my-python-identifier-re)
  "A regexp which matches the beginning of a function definition. The name of
  the function is stored in the first group.")

(defun my-python-save-func-signature nil
  "Insert into the kill ring the signature of the function at point"
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (looking-at "[ \t]*\\(.*\\):")
    (kill-new (match-string-no-properties 1))))

(defun my-python-save-name nil
  "Insert into the kill ring the name of the function at point"
  (interactive)
  (kill-new (my-python-definition-name)))

(defun my-python-definition-name nil
  "Return the name of the definition at point. This could be a class definition
or a function definition."
  (save-excursion
    (beginning-of-defun)
    (or (looking-at my-python-def-re)
        (looking-at my-python-class-re))
    (match-string-no-properties 1)))

(defun my-python-save-method-path nil
  "Insert into the kill ring a string of the form
  \"CLASS_NAME.METHOD_NAME\". When the function at point is not a method in a
  class, just store the function name."
  (interactive)
  (save-excursion
    (let (func-name class-name matched-p)
      (beginning-of-line)
      (unless (looking-at my-python-def-re)
        (beginning-of-defun)
        (unless (looking-at my-python-def-re)
          (error "Point not in function definition")))
      (setq func-name (match-string-no-properties 1))
      (while (and (my-python-goto-parent-line)
                  (not (setq matched-p (looking-at my-python-class-re)))))
      (when matched-p
        (setq class-name (match-string-no-properties 1)))
      (kill-new (if class-name (format "%s.%s" class-name func-name)
                  func-name)))))

(defvar my-python-defs-map (make-sparse-keymap))
(progn
  (define-key my-python-defs-map "n" 'my-python-save-name)
  (define-key my-python-defs-map "s" 'my-python-save-func-signature)
  (define-key my-python-defs-map "m" 'my-python-save-method-path))
(define-key python-mode-map "\C-cd" my-python-defs-map)

;; ----------------------------------------
;; search

(defun my-python-isearch-def nil
  (interactive)
  (let ((isearch-filter-predicate
         (lambda (beg end)
           (save-match-data
             (save-excursion
               (beginning-of-line)
               (and (looking-at my-python-def-re)
                    (<= (match-beginning 1) beg end (match-end 1))))))))
    (isearch-mode :forward nil nil t)))

(define-key python-mode-map (kbd "M-s d") 'my-python-isearch-def)

;; ----------------------------------------
;; misc

(defun my-python-goto-parent-line nil
  "Move to the beginning of the nearest preceding line having lower indentation
than the current one and return t. If no such line, keep point in place and
return nil."
  (let ((indentation (current-indentation))
        pos)
    (save-excursion
      (while (and (not (bobp))
                  (progn (beginning-of-line 0)
                         (or (looking-at my-blank-line-re)
                             (>= (current-indentation) indentation)
                             (progn (setq pos (point)) nil))))))
    (when pos (goto-char pos))))

(defun my-python-defs nil
  "Return a list of the names of the functions defined in the current buffer"
  (save-excursion
    (let (result)
      (beginning-of-buffer)
      (while (re-search-forward my-python-def-re nil t)
        (push (match-string-no-properties 1) result))
      result)))
;; ----------------------------------------
;; elpy
(setq elpy-modules nil)
(setq elpy-rpc-python-command "python3")
(elpy-enable)

;; ----------------------------------------
(provide 'my-python)
