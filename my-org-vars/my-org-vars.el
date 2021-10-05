(defvar my-org-vars--file
  (concat user-emacs-directory "my-org-vars/my-org-vars--file")
  "The path to the file which stores the bindings")

(defvar my-org-vars-alist nil
  "The name to node ID mapping")

;; * persistence

(defun my-org-vars--load nil
  (let (buffer)
    (with-current-buffer (setq buffer (find-file-noselect my-org-vars--file))
      (ignore-error 'end-of-file (setq my-org-vars-alist (read buffer)))
    (kill-buffer buffer))))

(defun my-org-vars--flush nil
  (with-temp-file my-org-vars--file
    (prin1 my-org-vars-alist (current-buffer))))

;; * commands

(defun my-org-vars-set (name)
  "Bind NAME to the node at point"
  (interactive (list (read-string "Identifier: ")))
  (let (binding (id (org-id-get-create)))
    (if (setq binding (assoc name my-org-vars-alist))
        (setcdr binding id)
      (push (cons name id) my-org-vars-alist)))
  (my-org-vars--flush))

(defun my-org-vars-goto (name)
  "Jumps to the node bound to NAME. Signals `void-variable' error when NAME is
not bound to any node."
  (interactive (list (completing-read
                      "Identifier: " (mapcar 'car my-org-vars-alist))))
  (let ((id (cdr (assoc name my-org-vars-alist))))
    (org-id-open id nil)))

;; * initialization

(my-org-vars--load)
