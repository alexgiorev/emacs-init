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
  (declare (indent 0))
  (unless (= (point-min) (point-max))
    (save-excursion
      (setq end (copy-marker end))
      (unwind-protect
          (progn
            (goto-char beg) (beginning-of-line)
            (while (when (<= (point) end)
                     (save-excursion (funcall fun))
                     (beginning-of-line 2)
                     (not (eobp)))))
        (set-marker end nil)))))
(put 'my-maplines 'lisp-indent-function 2)

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

(defsubst my-randchoice (list)
  "Return a random element from LIST or nil when empty"
  (when list
    (nth (random (length list)) list)))

(defun my-random-line nil
  (interactive)
  (my-random-point)
  (beginning-of-line))

(defun my-random-point nil
  (interactive)
  (goto-char (+ (point-min)
                (1+ (random (1+ (- (point-max) (point-min))))))))


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

;; ----------------------------------------
;; * alists

;; the utility of this function is that it enables the caller to see if
;; something was actually removed
(defun my-alist-pop (key alist-sym test)
  "Remove from the alist stored in ALIST-SYM the first item whose key equals
KEY, as determined by TEST. Return the removed item or nil if nothing was
removed."
  (let ((alist (symbol-value alist-sym))
        prev current)
    (when alist
      (if (funcall test key (caar alist))
          (progn (set alist-sym (cdr alist))
                 (car alist))
        (setq prev alist
              current (cdr alist))
        (while (and current (not (funcall test (caar current) key)))
          (setq prev current
                current (cdr current)))
        (when current
          (setcdr prev (cdr current))
          (car current))))))
;; ----------------------------------------
;; * lists
(defun my-list-index (elt list &optional test)
  "Return the index in LIST where ELT first appears.
The comparison is done with TEST, which defaults to `eq'."
  (let ((result 0)
        (current list)
        (test (or test 'eq))
        foundp)
    (while (and current (not (setq foundp (eq (car current) elt))))
      (setq result (1+ result)
            current (cdr current)))
    (and foundp result)))

(defun my-list-prev-cons (list pred)
  "Return the CONS preceding the one whose CAR passes PRED. Returns nil when
such a CONS is not present in LIST.

The first element of LIST is ignored. For example, the first element may pass
PRED but if no other element passes it then nil will be returned."
  (let ((current list) next result)
    (catch :break
      (my-loop-cons (current list)
        (when (setq next (cdr current))
          (when (funcall pred (car next))
            (setq result current) (throw :break nil)))))
    result))

(defun my-list-neighbor (list elt &optional left)
  "Return the neighbor of ELT in LIST or nil when ELT is not in LIST. This is
either the left neighbor when LEFT is non-nil or the right one. The right
neighbor of the last element is the first element, and the left neighbor of the
first element is the last element."
  (cond (left
         (if (eq (car list) elt)
             (car (last list))
           (car (my-list-prev-cons
                 list (lambda (other-elt) (eq elt other-elt))))))
          
        (t
         (let ((tail (memq elt list)))
           (when tail
             (if (cdr tail) (cadr tail) (car list)))))))

;; ----------------------------------------
;; lines
(defvar my-blank-line-re "^[ \t]*$"
  "Regexp which matches a blank line")

;; ----------------------------------------
;; loops

(defmacro my-loop-cons (var-list &rest body)
  "Iterate over the pairs of a list. For each pair, bind VAR to it and execute
BODY."
  (declare (indent 1))
  (let ((var (car var-list))
        (list-expr (cadr var-list)))
    `(let ((,var ,list-expr))
       (while ,var
         ,@body
         (setq ,var (cdr ,var))))))

(defvar do-while--break-symbol (make-symbol ":do-while--break"))
(defmacro do-while (&rest body)
  "Keep evaluating BODY until `end-do-while' is called"
  (declare (indent 0))
  `(catch do-while--break-symbol
     (while t
       ,@body)))
(defsubst end-do-while (&optional result)
  "End the nearest enclosing `do-while' loop"
  (throw do-while--break-symbol result))

;; ----------------------------------------
;; highlight
(defun my-highlight-region (start end)
  (interactive "r")
  (let ((overlay (make-overlay start end (current-buffer))))
    (overlay-put overlay 'face '(:foreground "white" :background "black"))
    (overlay-put overlay :my-highlight t)
    (overlay-put overlay 'evaporate t)))
(defun my-unhighlight-all nil
  (interactive)
  (remove-overlays (point-min) (point-max) :my-highlight t))
