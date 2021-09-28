(require 'my-macs)

;; ----------------------------------------
;; time utilities

(defun my-sched--today (&optional time)
  (let ((my-epoch (- (or time (my-int-time))
                     (- 86400 (car (current-time-zone))))))
    (/ my-epoch 86400)))

;; ----------------------------------------
;; persistent-storage

(defvar my-sched--data nil)
(defvar my-sched--data-file "~/.emacs.d/my-sched/data")
(defvar my-sched--data-buffer nil
  "The buffer which stores serialized scheduling data. It visits `my-sched--data-file'.")
(defvar my-sched--id-regexp-format ":id +%S"
  "Useful for finding the text of the record corresponding to the entry having a
particular ID")

(defun my-sched--find-sched (eid)
  "Positions point before the opening parenthesis of the plist corresponding to
the entry having EID as its id."
  (beginning-of-buffer)
  (when (re-search-forward (format my-sched--id-regexp-format eid) nil t)
    (search-backward "(")))

(defun my-sched--goto-sched (sched)
  (my-sched--find-sched (plist-get sched :id)))

(defun my-sched--flush-sched (sched)
  (with-current-buffer my-sched--data-buffer
    (if (my-sched--goto-sched sched)
        (progn (delete-region
                (point) (progn (forward-sexp) (point)))
               (prin1 sched (current-buffer)))
      (end-of-buffer)
      (prin1 sched (current-buffer)))
    (save-buffer)))

(defun my-sched--load-maybe nil
  "Load scheduling data from the file if they are not already loaded"
  (unless my-sched--data
    (setq my-sched--data-buffer (find-file-noselect my-sched--data-file))
    (with-current-buffer my-sched--data-buffer
      (emacs-lisp-mode)
      (rename-buffer "my-sched--data-buffer")
      (setq my-sched--data (my-read-buffer)))))

(defun my-sched--reload nil
  (setq my-sched--data nil)
  (kill-buffer my-sched--data-buffer)
  (setq my-sched--data-buffer nil)
  (my-sched--load-maybe)
  nil)

;; ----------------------------------------
;; Scheduling

(defun my-sched--get-sched-create (eid)
  "Return the scheduling record for the entry having id EID. If a record under
that id doesn't exist, create one, attach it to the scheduling data and return
it. This function does not flush the data."
  (let ((sched (seq-find (lambda (sched) (equal eid (plist-get sched :id)))
                         my-sched--data)))
    (unless sched
      (setq sched `(:id ,eid :due nil :interval nil))
      (add-to-list 'my-sched--data sched :append))
    sched))

(defun my-sched--schedule (eid interval)
  "Schedule the entry at EID for INTERVAL days into the future. Assumes
(integerp INTERVAL)"
  (let* ((sched (my-sched--get-sched-create eid))
         (interval (max 0 interval))
         (day (+ (my-sched--today) interval)))
    (plist-put sched :due day)
    (plist-put sched :interval interval)
    (my-sched--flush-sched sched)))

(defun my-sched--due-today nil
  "Return a list of the records which are due today"
  (let* ((today (my-sched--today)))
    (seq-filter
     (lambda (record) (<= (plist-get record :due) today))
     my-sched--data)))

;; ----------------------------------------
;; commands

(defun my-sched-schedule nil
  "Ask the user for a number and schedule the entry at point for that many days
into the future"
  (interactive)
  TODO)

(defvar my-sched-ring nil
  "A ring of entry ids.
Typically holds the ids of due entries so that the user can read what is due for
today.")

(defun my-sched-ring-init nil
  (interactive)
  (let ((ids (mapcar (lambda (sched) (plist-get sched :id))
                     (my-sched--due-today))))
    (when ids
      (my-circlist-make ids))
    (setq my-sched-ring ids)))

(defun my-sched-ring--check nil
  (if (not my-sched-ring)
      (user-error "Scheduling ring is empty")))
  
(defun my-sched-ring-goto-current nil
  (interactive)
  (my-sched-ring--check)
  (org-id-open (car my-sched-ring) nil))

(defun my-sched-ring-goto-next nil
  (interactive)
  (my-sched-ring--check)
  (setq my-sched-ring (cdr my-sched-ring))
  (my-sched-ring-goto-current))

(defun my-sched-ring-goto-prev nil
  (interactive)
  (my-sched-ring--check)
  (setq my-sched-ring (my-circlist-prev my-sched-ring))
  (my-sched-ring-goto-current))

(defun my-sched-ring-remove-current nil
  (interactive)
  (let ((prev (my-circlist-prev my-sched-ring)))
    (if (eq prev my-sched-ring)
        (setq my-sched-ring nil)
      (setcdr prev (cdr my-sched-ring))
      (setq my-sched-ring (cdr prev)))))

;; ----------------------------------------
;; initialization
(my-sched--load-maybe)
