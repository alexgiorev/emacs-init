(require 'my-macs)

(defvar my-org-vars-file
  (concat user-emacs-directory "lisp/my-org-vars/my-org-vars-alist")
"The path to the file which stores the bindings")

(defvar my-org-vars-alist nil
  "The name to node ID mapping")

;; persistence
;; ════════════════════════════════════════
(defun my-org-vars-load nil
  (let (buffer)
    (ignore-error 'file-error
      (with-current-buffer (setq buffer (find-file-noselect my-org-vars-file))
        (ignore-error 'end-of-file (setq my-org-vars-alist (read buffer)))
        (kill-buffer buffer)))))

(defun my-org-vars-flush nil
  (with-temp-file my-org-vars-file
    (prin1 my-org-vars-alist (current-buffer))))

;; utils
;; ════════════════════════════════════════

(defsubst my-org-vars-get (name)
  (cdr (assoc name my-org-vars-alist)))

(defun my-org-vars-read-var (&optional require-match initial-input)
  (completing-read "Identifier: "
                   (my-alist-keys my-org-vars-alist)
                   nil require-match initial-input))

;; commands
;; ════════════════════════════════════════

(defun my-org-vars-set (&optional name)
  "Bind NAME to the node at point"
  (interactive)
  (setq name (or name (completing-read "Identifier: " (mapcar 'car my-org-vars-alist)
                       nil nil
                       (org-no-properties (org-get-heading t t t t)))))
  (when (or (not (my-org-vars-get name))
             (string= (read-answer (format "\"%s\" is already used. Override? " name)
                                   '(("yes") ("no")))
                      "yes"))
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
  (let* ((name (completing-read "Identifier: " my-org-vars-alist))
         (id (my-org-vars-get name))
         location buffer)
    (unless id
      (user-error "\"%s\" is not bound" name))
    (unless (setq location (org-id-find id))
      (user-error "Cannot find \"%s\"" name))
    ;; get the buffer
    (setq buffer (cdr (assoc name my-org-vars-goto-alist)))
    (unless (buffer-live-p buffer)
      (assoc-delete-all name my-org-vars-goto-alist 'string=)
      (setq buffer nil))
    (unless buffer
      (if (setq buffer (get-file-buffer (car location)))
          (progn (setq buffer (make-indirect-buffer
                               buffer (generate-new-buffer-name name) :clone))
                 (push (cons name buffer)
                       my-org-vars-goto-alist))
        (setq buffer (find-file-noselect (car location)))))
    ;; switch to the buffer
    (switch-to-buffer buffer)
    (widen)
    (goto-char (cdr location))
    (when (invisible-p (point))
      (org-flag-heading nil))    
    (org-narrow-to-subtree)))

(defun my-org-vars-unset (name)
  (interactive (list (my-org-vars-read-var)))
  (let ((removed-item (my-alist-pop name 'my-org-vars-alist 'string=)))
    (if removed-item
        (progn (message "Removed \"%s\"" name)
               (my-org-vars-flush))
      (message "\"%s\" is not bound" name))))

(defun my-org-vars-insert-link (name)
  (interactive (list (my-org-vars-read-var
                      :require-match
                      (and (org-region-active-p)
                           (buffer-substring
                            (region-beginning)
                            (region-end))))))
  (let ((id (my-org-vars-get name))
        (text (if (org-region-active-p)
                  (delete-and-extract-region (region-beginning) (region-end))
                name)))
    (insert (org-link-make-string (concat "id:" id) text))))

(defun my-org-vars-link-region (start end)
  "For each occurence of an identifier in the region,
 replace it with a link to the variable"
  (interactive "r")
  (let ((re (concat "\\("
                    (regexp-opt (my-alist-keys my-org-vars-alist)
                                'words)
                    "\\)"))
        (case-fold-search nil)
        name id link)
    (org-with-wide-buffer
     (goto-char start)
     (narrow-to-region start end)
     (while (re-search-forward re nil t)
       (setq name (match-string 1)
             id (cdr (assoc name my-org-vars-alist))
             link (format "[[id:%s][%s]]" id name))
       (replace-match link)))))

(defun my-org-vars-rename (from to)
  (interactive
   (let ((from (completing-read "Identifier to rename: " my-org-vars-alist)))
     (unless (assoc from my-org-vars-alist)
       (user-error "Invalid identifier: %s" from))
     (list from (read-string (format "Rename \"%s\" to: " from)))))
  (let ((name+id (assoc from my-org-vars-alist)))
    (unless name+id
      (user-error "Invalid identifier: %s" from))
    (setcar name+id to)
    (my-org-vars-flush)))
  
;; keymap

;; ════════════════════════════════════════

(defvar my-org-vars-map (make-sparse-keymap))
(progn
  (define-key my-org-vars-map "g" 'my-org-vars-goto)
  (define-key my-org-vars-map "s" 'my-org-vars-set)
  (define-key my-org-vars-map "u" 'my-org-vars-unset)
  (define-key my-org-vars-map "l" 'my-org-vars-insert-link)
  (define-key my-org-vars-map "r" 'my-org-vars-link-region))
(define-key global-map "\C-xv" my-org-vars-map)

;; * initialization
;; ════════════════════════════════════════

(my-org-vars-load)
(provide 'my-org-vars)
