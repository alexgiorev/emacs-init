(defun shuffled-number-sequence (low high)
  "Returns a list of the numbers in the interval [LOW HIGH] but shuffled
randomly"
  (if (> low high)
      (error "LOW (%s) must be less than or equal to HIGH (%s)" low high))
  (let* ((vec (vector-number-sequence low high))
         (size (length vec))
         random-index)
    (dotimes (N (1- (length vec)))
      (setq random-index (random (- size N)))
      (vector-swap vec random-index (- size 1 N)))
    (append vec nil)))

(defsubst vector-swap (vec i1 i2)
  (let ((v1 (aref vec i1)) (v2 (aref vec i2)))
    (aset vec i1 v2) (aset vec i2 v1)))
    
(defun vector-number-sequence (low high)
  (if (> low high)
      (error "LOW (%s) must be less than or equal to HIGH (%s)" low high))
  (let* ((size (- high low -1))
         (result (make-vector size nil)))
    (dotimes (index size)
      (aset result index (+ low index)))
    result))

(defun my-touch-left (beg end)
  (interactive "r")
  (let ((mini (my-min-indentation beg end)))
    (indent-rigidly beg end (- mini))))

(defsubst my-min-indentation (beg end)
  "Return the minimum indentation among the lines delimited by BEG and END.
Ignores blank lines."
  (let ((result 1000))
    (my-maplines
     beg end
     (lambda ()
       (unless (looking-at "^[[:blank:]]*$")
         (setq result (min result (current-indentation))))))
    result))

(defun my-reduce-indentation (amount)
  "Reduce the indentation of the current line by AMOUNT.
Assumes that point is at the beginning of the line."
  (let ((start (point)))
    (skip-chars-forward "[[:blank:]]" (+ start amount))
    (delete-region start (point))))

(defun my-maplines (beg end fun)
  (unless (= (point-min) (point-max))
    (save-excursion
      (setq end (copy-marker end))
      (unwind-protect
          (progn
            (goto-char beg) (beginning-of-line)
            (while (when (<= (point) end)
                     (save-excursion (funcall fun))
                     (forward-line)
                     (not (eobp)))))
        (set-marker end nil)))))

(defun delete-current-line ()
  (interactive)
  (delete-region (progn (beginning-of-line) (point))
                 (progn (end-of-line)
                        (if (not (eobp)) (forward-char))
                        (point))))

(defun line-length (&optional N)
  (setq N (or N 0))
  (save-excursion
    (forward-line N)
    (- (progn (end-of-line) (point))
       (progn (beginning-of-line) (point)))))

(defun my-count-preceding-empty-lines ()
  (save-excursion
    (let ((empty-lines 0))
      (beginning-of-line 0)
      (while (when (looking-at "^\n")
               (cl-incf empty-lines)
               (unless (bobp) 
                 (beginning-of-line 0)
                 :continue-loop)))
      empty-lines)))

;; ----------------------------------------
(defun my-read-buffer (&optional buffer)
  "Return a list of the top-level forms in the current buffer. When omitted or
nil, use the current buffer."
  (let ((buffer (or buffer (current-buffer)))
        result)
    (with-current-buffer buffer
      (save-excursion
        (beginning-of-buffer)
        (ignore-error end-of-file
          (while t
            (setq result (cons (read buffer) result))))))
    (reverse result)))

(defun my-read-file (file)
  "Read the file at FILE and return the list of top-level forms"
  (with-temp-buffer
    (insert-file-contents file)
    (my-read-buffer)))

(provide 'my-macs)

;; ----------------------------------------

(defsubst my-int-time nil
  (time-convert (current-time) 'integer))

;; ----------------------------------------
(defsubst my-randint (low high)
  "Return a random number from the interval [low high] (i.e. from low to high inclusive)"
  (cond ((> low high)
         (error (concat "LOW must be less than or equal to HIGH, "
                        "but was given %s for LOW and %s for HIGH)")
                low high))
        ((= low high) low)
        (t (+ low (random (- high low -1))))))

;; ----------------------------------------
;; circular lists

(defun my-circlist-make (list)
  "Turns LIST into a circular lists"
  (when list (setcdr (last list) list)))

(defun my-circlist-prev (list)
  (let ((current list))
    (while (not (eq (cdr current) list))
      (setq current (cdr current)))
    current))

(defun my-circlist-add (clist element &optional before)
  "Assumes that CLIST is a cons which is a node in a circular list. Inserts
ELEMENT after CLIST and returns the new cons. Optional argument BEFORE controls
whether the element is inserted before or after CLIST in the circular list."
  (let* ((prev (if before (my-circlist-prev clist) clist))
         (next (cdr prev))
         (new (cons element next)))
    (setcdr prev new)
    new))

(defun my-circlist-pop (var)
  "Remove the head of the circluar list stored at VAR and position VAR on the next element."
  (let (clist prev value)
    (unless (setq clist (symbol-value var))
      (error "Cannot pop from an empty circular list"))
    (setq prev (my-circlist-prev clist)
          head (car clist))
    (if (eq prev clist)
        (set var nil)
      (setcdr prev (cdr clist))
      (set var (cdr prev)))
    head))
      
;; ----------------------------------------
(defun my-jump-to-marker (marker)
  (unless (marker-buffer marker)
    (error "Cannot jump to a marker whose buffer is nil"))
  (unless (marker-position marker)
    (error "Cannot jump to a marker whose position is nil"))
  (switch-to-buffer (marker-buffer marker))
  (goto-char marker))

;; ----------------------------------------
(defsubst my-plist-foreach (func plist)
  "Call FUNC on each (key,value) pair in PLIST and return nil.
FUNC should accept two arguments KEY and VALUE"
  (let ((current plist))
    (while current
      (funcall func (car current) (car (setq current  (cdr current))))
      (setq current (cdr current)))))

;; ----------------------------------------

(defun my-same-line (pos1 pos2)
  (save-excursion
    (= (progn (goto-char pos1) (line-beginning-position))
       (progn (goto-char pos2) (line-beginning-position)))))

;; ----------------------------------------

(defun my-buffer-overlay-substring (start end)
  "Returns the overlay string at the region (START END).
The format of an overlay string is a pair whose first element is a string and
whose second element is a list of triples (START END PROPS)"
  (let* ((overlays (overlays-in start end))
         ;; Skip the overlays which set the face, including the `region' face
         (overlays (seq-filter (lambda (overlay)
                                 (not (overlay-get overlay 'face)))
                               overlays))
         (overlays-data
          (mapcar (lambda (overlay)
                    (list (- (overlay-start overlay) start)
                          (- (overlay-end overlay) start)
                          (overlay-properties overlay)))
                  overlays)))
    (cons (buffer-substring-no-properties start end)
          overlays-data)))

(defun my-insert-overlay-string (ostr)
  (let ((string (car ostr))
        (triples (cdr ostr))
        (base (point))
        overlay)
    (save-excursion (insert string))
    (dolist (triple triples)
      (pcase-let ((`(,start ,end ,props) triple))
        (setq overlay (make-overlay (+ base start) (+ base end)))
        (my-plist-foreach
         (apply-partially 'overlay-put overlay)
         props)))))
