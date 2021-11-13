(require 'my-macs)

;;════════════════════════════════════════
;; persistent-storage

(defvar sched--queues-dir (concat user-emacs-directory "lisp/sched/queues/"))
(defvar sched--queues :unloaded
  "An alist of (name,queue) pairs or the symbol `:unloaded'.
There is one queue per file in `sched--queues-dir'")
(defvar sched--current-queue nil)

(defun sched--load-maybe nil
  "Load queues if they aren't loaded yet"
  (when (eq sched--queues :unloaded)
    (setq sched--queues
          (mapcar
           (lambda (path)
             (cons (file-name-sans-extension (file-name-nondirectory path))
                   (org-pplist-make path)))
           (directory-files sched--queues-dir :full "\\.queue$")))
    (setq sched--current-queue (car sched--queues))))

(defun sched--reload nil
  (setq sched--queues :unloaded)
  (sched--load-maybe))

;;════════════════════════════════════════
;; utils

(defun sched--create-sched (eid queue)
  "Create a new scheduling record for EID and return it"
  (org-pplist-add queue eid (list :due nil :interval nil)))

(defun sched--due-today (&optional queue)
  "Return a list of the records which are due today"
  (setq queue (or queue (cdr sched--current-queue)))
  (let* ((today (my-time-today)))
    (seq-filter
     (lambda (sched) (<= (plist-get sched :due) today))
     (plist-get queue :list))))

(defun sched--read-queue nil
  "Asks the user for a queue name and returns the object or nil if no queue by that name.
Assumes that the queues are loaded"
  (let* ((name (completing-read "Queue: " (my-alist-keys sched--queues)))
         (queue (cdr (assoc name sched--queues))))
    (unless queue (user-error "No queue with name %S" name))
    queue))

(defun sched--check nil
  (sched--load-maybe)
  (unless sched--queues (user-error "No queues")))

(defun sched--get-titles (eids)
  (seq-filter 'identity
              (mapcar (lambda (eid) (car (my-org-id-get eid '(title))))
                      eids)))

;;════════════════════════════════════════
;; * commands

(defun sched-make-queue nil
  (interactive)
  (sched--check)
  (let ((name (read-string "Queue name: "))
        queue)
    (when (assoc name sched--queues)
      (user-error "A queue with name %S already exists" name))
    (setq path (concat sched--queues-dir name ".queue"))
    (make-empty-file path)
    (setq queue (org-pplist-make path))
    (push (cons name queue) sched--queues)
    (message "Queue %S created successfully" name)
    queue))

(defvar sched-did-schedule-hook nil
  "A hook which is ran after an entry is scheduled.
The functions accept as an argument the ID of the entry that was scheduled")
(defun sched-schedule (arg)
  "Schedules the node node at point on the current queue. Asks the user for the interval.
When called with a prefix argument, ask the user for the queue as well"
  (interactive "P")
  (sched--check)
  (let* ((eid (org-id-get))
         (queue (if arg (sched--read-queue) (cdr sched--current-queue)))
         (sched (and eid (org-pplist-get queue eid)))
         (prompt (if sched
                     (format "Interval (last was %s): "
                             (plist-get sched :interval))
                   "Interval: "))
         (new-interval (read-number prompt)))
    (unless sched
      (setq eid (or eid (org-id-get-create)))
      (setq sched (sched--create-sched eid queue)))
    (sched--schedule sched new-interval)
    (org-pplist-updated-plist queue sched :flush)
    (run-hook-with-args 'sched-did-schedule-hook eid)))

(defun sched--schedule (sched interval)
  (let* ((interval (max 0 interval))
         (day (+ (my-time-today) interval)))
    (plist-put sched :due day)
    (plist-put sched :interval interval)))

(defun sched-remove (arg)
  "Remove the node at point from the current queue.
When called with a prefix argument, ask the user for the queue."
  (interactive "P")
  (let ((queue (if arg (sched--read-queue) (cdr sched--current-queue)))
        (eid (org-entry-get nil "ID")))
    (if (not eid)
        (message "Node not in queue")
      (if (org-pplist-remove queue eid)
          (message "Removed node")
        (message "Node not in queue")))))

(defun sched-select-queue nil
  (interactive)
  (sched--check)
  (let ((queue (org-select-list sched--queues)))
    (when queue (setq sched--current-queue queue))))

;; ════════════════════
;; ** ring commands

(defvar sched-ring (circlist-make nil)
  "A ring of entry ids.
Typically holds the ids of due entries so that the user can read what is due for
today.")

(defun sched-ring--check nil
  (when (circlist-empty-p sched-ring)
    (user-error "Ring is empty")))

(defun sched-ring-reset (arg)
  (interactive "P")
  (sched--check)
  (let* ((queue (if arg (sched--read-queue) (cdr sched--current-queue)))
         (ids (mapcar (lambda (sched) (plist-get sched :id))
                      (sched--due-today queue))))
    (setq sched-ring (circlist-make ids))
    (when (called-interactively-p 'interactive)
      (message "%s entries due" (circlist-length sched-ring)))))

(defun sched-ring-select nil
  (interactive)
  (sched-ring--check)
  (let* ((eids (circlist-to-list sched-ring))
         (headings (seq-filter 'identity
                               (mapcar (lambda (eid)
                                         (car (my-org-id-get eid '(heading))))
                                       eids)))
         (items (my-zip-alist headings eids))
         (eid (org-select-list items)))
    (when eid
      (while (not (string= (circlist-current sched-ring) eid))
        (circlist-rotate sched-ring :next))
      (sched-ring-jump))))

(defun sched-ring-jump nil
  (interactive)
  (sched-ring--check)
  (let (did-open)
    (while (and (not (circlist-empty-p sched-ring)) (not did-open))
      (ignore-errors
        (org-id-open (circlist-current sched-ring) nil)
        (setq did-open t))
      (unless did-open
        (circlist-pop sched-ring)))
    (unless did-open
      (sched-ring--check))))

(defun sched-ring-next nil
  (interactive)
  (sched-ring--check)
  (circlist-rotate sched-ring :next)
  (sched-ring-jump))

(defun sched-ring-prev nil
  (interactive)
  (sched-ring--check)
  (circlist-rotate sched-ring :prev)
  (sched-ring-jump))

(defun sched-ring-pop nil
  (interactive)
  (sched-ring--check)
  (let* ((eid (circlist-pop sched-ring))
         (title (my-org-id-get eid '(title))))
    (message "Popped %S" title)))

(defun sched-ring-after-node-scheduled (eid)
  (circlist-remove sched-ring (lambda (elt) (string= elt eid))))

;;════════════════════════════════════════
;; keymap

(defvar sched-map (make-sparse-keymap))
(progn
  (define-key sched-map "r" 'sched-ring-reset)
  (define-key sched-map "p" 'sched-ring-prev)
  (define-key sched-map "n" 'sched-ring-next)
  (define-key sched-map "j" 'sched-ring-jump)
  (define-key sched-map "o" 'sched-ring-pop)
  (define-key sched-map "s" 'sched-schedule)
  (define-key sched-map (kbd "DEL") 'sched-remove)
  (define-key sched-map "q" 'sched-select-queue)
  (define-key sched-map "e" 'sched-ring-select))
(define-key global-map "\C-xs" sched-map)

;;════════════════════════════════════════
(provide 'sched)
