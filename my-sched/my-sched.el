(require 'my-macs)

;;########################################
;; time utilities

(defun my-sched--today (&optional time)
  (let ((my-epoch (- (or time (my-int-time))
                     (- 86400 (car (current-time-zone))))))
    (/ my-epoch 86400)))

;;########################################
;; persistent-storage

(defvar my-sched--data nil)
(defvar my-sched--data-file (concat user-emacs-directory "my-sched/data"))
(defvar my-sched--data-buffer nil
  "The buffer which stores serialized scheduling data. It visits `my-sched--data-file'.")
(defvar my-sched--id-regexp-format ":id +%S"
  "Useful for finding the text of the record corresponding to the entry having a
particular ID")
(defvar my-sched--id-property (make-symbol "my-sched-id")
  "The property which associates a line in `my-sched--data-buffer' with an
ID. The ID is the same one you would get if you were to parse the line and
fetch the :id attribute")

(defun my-sched--find-sched (eid)
  "Positions point on the beginning of the line holding the scheduling
information corresponding to the entry having EID as its ID. If there is no such
line, returns nil. Otherwise, returns a non-nil value."
  (beginning-of-buffer)
  (when (text-property-search-forward my-sched--id-property eid 'string=)
    (beginning-of-line) :found))

(defsubst my-sched--goto-sched (sched)
  (my-sched--find-sched (plist-get sched :id)))

(defun my-sched--flush-sched (sched)
  (with-current-buffer my-sched--data-buffer
    (if (my-sched--goto-sched sched)
        (progn (delete-region
                (point) (line-end-position))
               (prin1 sched (current-buffer))
               (beginning-of-line))
      (end-of-buffer)
      (prin1 sched (current-buffer)) (insert "\n")
      (beginning-of-line 0))
    (my-sched--associate-line (plist-get sched :id))
    (write-region nil nil (buffer-file-name my-sched--data-buffer)
                  nil :no-write-message)
    ;; avoid query when killing
    (set-buffer-modified-p nil))
  (bury-buffer my-sched--data-buffer))

(defun my-sched--load-maybe nil
  "Load scheduling data from the file if they are not already loaded"
  (unless my-sched--data
    (setq my-sched--data-buffer (find-file-noselect my-sched--data-file))
    (with-current-buffer my-sched--data-buffer
      (emacs-lisp-mode)
      (rename-buffer " *my-sched--data-buffer*")
      (setq my-sched--data (my-read-buffer))
      ;; associate lines with IDs
      (beginning-of-buffer)
      (dolist (sched my-sched--data)
        (my-sched--associate-line (plist-get sched :id))
        (beginning-of-line 2))
      ;; avoid query when killing
      (set-buffer-modified-p nil))))

(defun my-sched--reload nil
  (setq my-sched--data nil)
  (kill-buffer my-sched--data-buffer)
  (setq my-sched--data-buffer nil)
  (my-sched--load-maybe)
  nil)

(defun my-sched--associate-line (id)
  "Associate the current line in `my-sched--data-buffer' with ID. Assumes that
point is on the beginning of the line."
  (put-text-property (point) (1+ (point))
                     my-sched--id-property id))

;;########################################
;; Scheduling

(defun my-sched--get-sched (eid)
  "Return the scheduling record for the entry having id EID. If there is no
scheduling data for that EID, return nil. If EID is nil, return nil."
  (when eid
    (seq-find (lambda (sched) (equal eid (plist-get sched :id)))
              my-sched--data)))

(defun my-sched--create-sched (eid)
  "Create a new scheduling record and insert into the `my-sched--data'. Assumes
that there is no record under EID."
  (let ((sched (list :id eid :due nil :interval nil)))
    (add-to-list 'my-sched--data sched :append)
    sched))

(defun my-sched--get-sched-create (eid)
  "A facility function. Return the scheduling record for the entry having id
EID. If a record under that id doesn't exist, create one, attach it to the
scheduling data and return it."
  (unless eid
    (error "EID cannot be nil"))
  (let ((sched (my-sched--get-sched eid)))
    (unless sched
      (setq sched (my-sched--create-sched eid)))
    sched))
    
(defun my-sched--schedule (sched interval)
  "Schedule SCHED for INTERVAL days from today. Assumes that SCHED is already in
the data. Flushes the data."
  (let* ((interval (max 0 interval))
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

;;########################################
;; * commands

(defun my-sched-schedule (&optional interval)
  "Schedule the entry at point INTERVAL days into the future. When INTERVAL is nil, ask the user"
  (interactive)
  (let* ((eid (org-id-get))
         (sched (my-sched--get-sched eid))
         (prompt (if sched
                     (format "Interval (last was %s): "
                             (plist-get sched :interval))
                   "Interval: "))
         (new-interval (or interval (read-number prompt))))
    (unless sched
      (setq eid (org-id-get-create))
      (setq sched (my-sched--create-sched eid)))
    (my-sched--schedule sched new-interval)))

;; ** ring commands

(defvar my-sched-ring nil
  "A ring of entry ids.
Typically holds the ids of due entries so that the user can read what is due for
today.")

(defun my-sched-ring-reset nil
  (interactive)
  (let* ((ids (mapcar (lambda (sched) (plist-get sched :id))
                      (my-sched--due-today)))
         (length (length ids)))
    (if ids
        (setq my-sched-ring (my-circlist-make ids))
      (setq my-sched-ring nil))
    (when (called-interactively-p 'interactive)
      (message "%s entries due" length))))

(defun my-sched-ring--check nil
  (if (not my-sched-ring)
      (user-error "Ring is empty")))

(defun my-sched-ring-jump nil
  (interactive)
  (my-sched-ring--check)
  (org-id-open (car my-sched-ring) nil))

(defun my-sched-ring-jump nil
  (interactive)
  (let (did-open)
  (while (and my-sched-ring (not did-open))
    (ignore-errors
      (org-id-open (car my-sched-ring) nil)
      (setq did-open t))
    (unless did-open
      (my-circlist-pop 'my-sched-ring)))
  (unless did-open
    (my-sched-ring--check))))

(defun my-sched-ring-next nil
  (interactive)
  (my-sched-ring--check)
  (setq my-sched-ring (cdr my-sched-ring))
  (my-sched-ring-jump))

(defun my-sched-ring-prev nil
  (interactive)
  (my-sched-ring--check)
  (setq my-sched-ring (my-circlist-prev my-sched-ring))
  (my-sched-ring-jump))

(defun my-sched-ring-pop nil
  (interactive)
  (my-sched-ring--check)
  (my-circlist-pop 'my-sched-ring)
  (when (called-interactively-p 'interactive)
    (message "Popped link")))

;;----------------------------------------
;; keymap

(defvar my-sched-map (make-sparse-keymap))
(progn
  (define-key my-sched-map "r" 'my-sched-ring-reset)
  (define-key my-sched-map "p" 'my-sched-ring-prev)
  (define-key my-sched-map "n" 'my-sched-ring-next)
  (define-key my-sched-map "j" 'my-sched-ring-jump)
  (define-key my-sched-map "o" 'my-sched-ring-pop)
  (define-key my-sched-map "s" 'my-sched-schedule))

(define-key org-mode-map "\C-cs" my-sched-map)

;;########################################
(provide 'my-sched)

;;########################################
;; initialization

(my-sched--load-maybe)
