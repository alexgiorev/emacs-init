(require 'my-macs)

(defvar my-org-vars-file
  (concat user-emacs-directory "my-org-vars/my-org-vars-alist")
  "The path to the file which stores the bindings")

(defvar my-org-vars-alist nil
  "The name to node ID mapping")

;; * persistence

(defun my-org-vars-load nil
  (let (buffer)
    (ignore-error 'file-error
      (with-current-buffer (setq buffer (find-file-noselect my-org-vars-file))
        (ignore-error 'end-of-file (setq my-org-vars-alist (read buffer)))
        (kill-buffer buffer)))))

(defun my-org-vars-flush nil
  (with-temp-file my-org-vars-file
    (prin1 my-org-vars-alist (current-buffer))))

;; * utils

(defsubst my-org-vars-read-var nil
  (completing-read "Identifier: " (mapcar 'car my-org-vars-alist)))

(defsubst my-org-vars-get (name)
  (cdr (assoc name my-org-vars-alist)))

;; * commands

(defun my-org-vars-set (name)
  "Bind NAME to the node at point"
  (interactive (list (my-org-vars-read-var)))
  (when (or (not (my-org-vars-get name))
             (string= (read-answer (format "\"%s\" is already used. Override? " name)
                                   '(("yes") ("no")))))
    (let ((binding nil) (id (org-id-get-create)))
      (if (setq binding (assoc name my-org-vars-alist))
          (setcdr binding id)
        (push (cons name id) my-org-vars-alist)))
    (my-org-vars-flush)))

(defvar my-org-vars-goto-alist nil
  "Maps names to the buffers which should be used when going to the names using
  `my-org-vars-goto'")
(defun my-org-vars-goto nil
  "Jumps to the node bound to NAME. Signals `void-variable' error when NAME is
not bound to any node."
  (interactive)
  (let* ((name (my-org-vars-read-var))
         (id (my-org-vars-get name))
         location buffer)
    (unless id
      (user-error "\"%s\" is not bound" name))
    (unless (setq location (org-id-find id))
      (user-error "Cannot find \"%s\"" name))
    ;; get the buffer
    (setq buffer (cdr (assoc name my-org-vars-goto-indirect-buffer-alist)))
    (unless (buffer-live-p buffer)
      (assoc-delete-all
       name my-org-vars-goto-indirect-buffer-alist 'string=)
      (setq buffer nil))
    (unless buffer
      (if (setq buffer (get-file-buffer (car location)))
          (progn (setq buffer (make-indirect-buffer
                               buffer (generate-new-buffer-name name) :clone))
                 (push (cons name buffer)
                       my-org-vars-goto-indirect-buffer-alist))
        (setq buffer (find-file-noselect (car location)))))
    ;; switch to the buffer
    (switch-to-buffer buffer)
    (widen)
    (goto-char (cdr location))
    (org-narrow-to-subtree)))

(defun my-org-vars-unset (name)
  (interactive (list (my-org-vars-read-var)))
  (let ((removed-item (my-alist-pop name 'my-org-vars-alist 'string=)))
    (if removed-item
        (progn (message "Removed \"%s\"" name)
               (my-org-vars-flush))
      (message "\"%s\" is not bound" name))))

(defun my-org-vars-insert-link (name)
  (interactive (list (my-org-vars-read-var)))
  (let ((id (my-org-vars-get name)))
    (insert (org-link-make-string (concat "id:" id) name))))

;; * keymap

(defvar my-org-vars-map (make-sparse-keymap))
(progn
  (define-key my-org-vars-map "g" 'my-org-vars-goto)
  (define-key my-org-vars-map "s" 'my-org-vars-set)
  (define-key my-org-vars-map "u" 'my-org-vars-unset)
  (define-key my-org-vars-map "l" 'my-org-vars-insert-link))
(define-key org-mode-map "\C-cv" my-org-vars-map)

;; * initialization

(my-org-vars-load)
(provide 'my-org-vars)
