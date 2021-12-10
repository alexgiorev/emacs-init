(require 'my-macs)
;;════════════════════════════════════════
(defvar cpath-forest (forest))

;;════════════════════════════════════════
;; misc

(defsubst cpath--check nil
  "Raises an error when a call path is not active"
  (condition-case nil
      (forest--check cpath-forest)
    (error (error "Not on a call path"))))

(defsubst cpath--jump nil
  (let ((current (forest-current cpath-forest)))
    (when current
      (my-jump-to-marker (plist-get current :marker)))))

;;════════════════════════════════════════
;; commands

(defun cpath-call (&optional arg)
  "\"Calls\" the function at point, which technically means that a child to the
current node is created which corresponds to the function at point. When called
with a prefix argument, makes a top-level call."
  (interactive "P")
  (let ((name (funcall cpath-func-name-func))
        (marker (point-marker)))
    (cond ((equal arg '(4))
           (forest-new-root cpath-forest :marker marker :name name))
          ((equal arg '(16))
           (forest-new-parent cpath-forest :marker marker :name name))
          (t
           (forest-new-child cpath-forest :marker marker :name name)))
    (message "Called %s" name)))

(defun cpath-up nil
  "Go to and mark as current the caller of the current node"
  (interactive)
  (cpath--check)
  (forest-goto-parent cpath-forest)
  (cpath--jump))

(defun cpath-down nil
  (interactive)
  (cpath--check)
  (forest-goto-child cpath-forest)
  (cpath--jump))

(defun cpath-goto-current nil
  (interactive)
  (cpath--check)
  (cpath--jump))

(defun cpath-prune nil
  "Deletes the current subtree and the parent becomes the new current node. When
the current node is a root, removes the whole tree and the current node becomes
the root of the first top-level tree."
  (interactive)
  (cpath--check)
  (forest-prune cpath-forest)
  (cpath--jump))

(defun cpath-select nil
  (interactive)
  (cpath--check)
  (when (forest-select cpath-forest)
    (cpath--jump)))

;;════════════════════════════════════════
;; keymap

(defvar cpath-map (make-sparse-keymap))
(progn
  (define-key cpath-map "c" 'cpath-goto-current)
  (define-key cpath-map "p" 'cpath-up)
  (define-key cpath-map "n" 'cpath-down)
  (define-key cpath-map "d" 'cpath-prune)
  (define-key cpath-map " " 'cpath-call)
  (define-key cpath-map "e" 'cpath-select))
(define-key prog-mode-map "\C-cp" cpath-map)

;;════════════════════════════════════════
(provide 'cpath)
