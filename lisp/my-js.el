(require 'js)
;;════════════════════════════════════════
(progn
  (define-key js-mode-map (kbd "C-c C-n") 'my-next-ruler)
  (define-key js-mode-map (kbd "C-c C-p") 'my-prev-ruler))

(defun my-js-insert-TODO nil
  (interactive)
  (insert "throw new Error(\"TODO\");"))
(define-key js-mode-map (kbd "C-c C-t") 'my-js-insert-TODO)

;;════════════════════════════════════════
(provide 'my-js)
