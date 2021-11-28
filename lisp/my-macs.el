;;════════════════════════════════════════
;; random

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

;;════════════════════════════════════════
;; vectors

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

;;════════════════════════════════════════
;; indendation

(defun my-touch-left (beg end)
  (interactive "r")
  (let ((mini (my-min-indentation beg end)))
    (indent-rigidly beg end (- mini))))

(defsubst my-min-indentation (beg end)
  "Return the minimum indentation among the lines delimited by BEG and END.
Ignores blank lines."
  (let ((result 1000))
    (my-maplines
        (lambda nil
          (unless (looking-at "^[[:blank:]]*$")
            (setq result (min result (current-indentation)))))
     beg end)
    result))

(defun my-reduce-indentation (amount)
  "Reduce the indentation of the current line by AMOUNT.
Assumes that point is at the beginning of the line."
  (let ((start (point)))
    (skip-chars-forward "[[:blank:]]" (+ start amount))
    (delete-region start (point))))

;; ════════════════════════════════════════
;; reading

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

;; ════════════════════════════════════════
;; time

(defsubst my-int-time nil
  (time-convert (current-time) 'integer))

(defsubst my-time-today (&optional time)
  (let ((my-epoch (- (or time (my-int-time))
                     (- 86400 (car (current-time-zone))))))
    (/ my-epoch 86400)))

;; ════════════════════════════════════════
;; circular lists (useful for implementing rings)

(defvar circlist-marker (make-symbol "circlist"))

(defsubst circlist-make (list)
  "Turns LIST into a circular lists"
  (record circlist-marker list list))
(defsubst circlist--elements (circlist)
  (aref circlist 1))
(defsubst circlist--set-elements (circlist elements)
  (aset circlist 1 elements))
(defsubst circlist--current-cons (circlist)
  (aref circlist 2))
(defsubst circlist--set-current-cons (circlist cell)
  (aset circlist 2 cell))

(defun circlist--prev-cons (circlist)
  (let ((elements (circlist--elements circlist))
        (current-cons (circlist--current-cons circlist)))
    (when elements
      (if (eq elements current-cons) (last elements)
        (catch :break
          (my-loop-cons (cell elements)
            (when (eq (cdr cell) current-cons)
              (throw :break cell))))))))

(defun circlist--next-cons (circlist)
  (let ((elements (circlist--elements circlist))
        (current-cons (circlist--current-cons circlist)))
    (when elements
      (if (cdr current-cons) (cdr current-cons) elements))))

(defsubst circlist-length (circlist)
  (length (circlist--elements circlist)))
(defsubst circlist-empty-p (circlist)
  (null (circlist--elements circlist)))
(defsubst circlist-current (circlist)
  (when (circlist-empty-p circlist)
    (error "circular list is empty"))
  (car (circlist--current-cons circlist)))

(defun circlist-add (circlist element &optional direction)
  "Inserts ELEMENT before (when DIRECTION is eq to :before or :prev) or after
(when DIRECTION is eq to :after or :next) the current CIRCLIST element"
  (let ((current-cons (circlist--current-cons circlist))
        (elements (circlist--elements circlist)))
    (cond ((null elements)
           (circlist--set-elements circlist (setq elements (list element)))
           (circlist--set-current-cons circlist elements))
          ((memq direction (list :before :prev))
           (if (eq current-cons elements)
               (circlist--set-elements
                circlist (setq elements (cons element elements)))
             (let ((prev-cons (circlist--prev-cons circlist)))
               (setcdr prev-cons (cons element current-cons)))))
          ((memq direction (list :after :next))
           (setcdr current-cons (cons element (cdr current-cons))))
          (t (error "Invalid DIRECTION argument: %S" direction)))))

(defun circlist-pop (circlist &optional direction)
  "Remove and return CIRCLIST's current element.
Set the current element based on DIRECTION, which defaults to :next"
  (let ((current (circlist--current-cons circlist))
        (elements (circlist--elements circlist))
        (next (circlist--next-cons circlist))
        (prev (circlist--prev-cons circlist)))    
    (cond ((not elements)
           (error "Cannot pop from an empty circular list"))
          ((null (cdr elements))
           (circlist--set-elements circlist nil)
           (circlist--set-current-cons circlist nil))
          ((eq current elements)
           (setq elements (cdr elements))
           (circlist--set-elements circlist elements)
           (circlist--set-current-cons circlist elements))
          (t (setcdr prev (cdr current))
             (circlist--set-current-cons
              circlist (if (memq direction (list :next :after)) next prev))))
    (car current)))

(defsubst circlist-to-list (circlist)
  (copy-sequence (circlist--elements circlist)))

(defun circlist-rotate (circlist direction)
  "DIRECTION must be one of (:next :prev)"
  (unless (circlist-empty-p circlist)
    (let ((func (if (eq direction :next) 'circlist--next-cons 'circlist--prev-cons)))
      (circlist--set-current-cons circlist (funcall func circlist)))))

(defun circlist-remove (circlist pred)
  "Returns a circlular list derived from CIRCLIST by removing all elements which
satisfy PRED. When the current element is also removed, the new current is
unspecified."
  (let ((current-cons (circlist--current-cons circlist))
        (new-elements (LIST-drop-m pred (circlist--elements circlist))))
    (circlist--set-elements circlist new-elements)
    (while (and current-cons (eq (car current-cons) :LIST-drop-m-did-remove))
      (setq current-cons (cdr current-cons)))
    (unless current-cons (setq current-cons new-elements))
    (circlist--set-current-cons circlist current-cons)))

;; ════════════════════════════════════════
;; plists

(defsubst my-plist-foreach (func plist)
  "Call FUNC on each (key,value) pair in PLIST and return nil.
FUNC should accept two arguments KEY and VALUE"
  (let ((current plist))
    (while current
      (funcall func (car current) (car (setq current  (cdr current))))
      (setq current (cdr current)))))

;; ════════════════════════════════════════
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

(defsubst my-alist-keys (alist)
  (mapcar 'car alist))

;; ════════════════════════════════════════
;; * lists
(defun LIST-index (elt list &optional test)
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

(defun LIST-prev-cons (list pred)
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

(defun LIST-neighbor (list elt direction &optional different)
  "Return the neighbor of ELT in LIST or nil when ELT is not in LIST. This is
either the previous neighbor when DIRECTION is :prev or the next one when
DIRECTION is :next. This function cycles the list, so that the next neighbor of
the last element is the first element, and the previous neighbor of the first
element is the last element. This means that when there is only one element, it
is its own previous and next neighbor. If you want the function to return nil
when LIST is a singleton, give a non-nil DIFFERENT argument"
  (unless (and different (not (cdr list)))
    (cond ((eq direction :prev)
           (if (eq (car list) elt)
               (car (last list))
             (car (LIST-prev-cons
                   list (lambda (other-elt) (eq elt other-elt))))))
          
          ((eq direction :next)
           (let ((tail (memq elt list)))
             (when tail
               (if (cdr tail) (cadr tail) (car list)))))
          (t (error "Invalid direction: %s" direction)))))

(defun LIST-add-after (list elt new)
  "Returns a list formed by adding NEW after ELT. Modifies LIST"
  (catch :break
    (my-loop-cons (cell list)
      (when (eq (car cell) elt)
        (setcdr cell (cons new (cdr cell)))
        (throw :break nil))))
  list)

(defun LIST-add-before (list elt new)
  "Returns a list formed by adding NEW before ELT. Modifies LIST (unless ELT is
the head of LIST)"
  (let ((cell (LIST-prev-cons list (lambda (x) (eq x elt)))))
    (if (not cell)
        (cons new list)
      (setcdr cell (cons new (cdr cell)))
      list)))

(defun LIST-add (list elt new direction)
  "Return a list formed by adding NEW before (when DIRECTION is :prev) or after
(when DIRECTION is :next) ELT."
  (cond ((eq direction :next) (LIST-add-after list elt new))
        ((eq direction :prev) (LIST-add-before list elt new))
        (t (error "Invalid direction: %s" direction))))

(defun LIST-remove (list-sym pred)
  "Remove from the list stored at LIST-SYM the first element which satisifes PRED.
Returns t when something was actually removed and nil otherwise."
  (let ((list (symbol-value list-sym)) (prev-cons nil))
    (when list
      (if (funcall pred (car list))
          (progn (set list-sym (cdr list)) t)
        (setq prev-cons (LIST-prev-cons list pred))
        (when prev-cons (setcdr prev-cons (cddr prev-cons)) t)))))

(defun LIST-delete-and-tell (list pred)
  (let (prev-cons)
    (when list
      (if (funcall pred (car list))
          (cons (cdr list) t)
        (setq prev-cons (LIST-prev-cons list pred))
        (if prev-cons
          (progn (setcdr prev-cons (cddr prev-cons))
                 (cons list t))
          (cons list nil))))))

(defun my-cell-in-list (cell list)
  "Return t or nil based on whether CELL is one of LIST's cons pairs"
  (catch :break
    (my-loop-cons (list-cell list)
      (when (eq cell list-cell)
        (throw :break t)))))

(defun LIST-drop-m (pred list)
  "Returns a list derived from LIST by dropping all elements which pass PRED.
This operation is destructive, no new cells are created. The cells which are
removed have their CAR changed to `:LIST-drop-m-did-remove'."
  (let* ((list (cons nil list))
         (current list) next)
    (while (setq next (cdr current))
      (if (funcall pred (car next))
          (progn (setcdr current (cdr next))
                 (setcar next :LIST-drop-m-did-remove))
        (setq current next)))
    (cdr list)))

;; ════════════════════════════════════════
;; loops

(defmacro my-loop-cons (var-and-list &rest body)
  "Iterate over the pairs of a list. For each pair, bind VAR to it and execute BODY. Always returns nil.
WARNING: If the list is circular, this will loop forever"
  (declare (indent 1))
  (let ((var (car var-and-list))
        (list-expr (cadr var-and-list)))
    `(let ((,var ,list-expr))
       (while ,var
         ,@body
         (setq ,var (cdr ,var))))))

(defvar loop--break-symbol (make-symbol ":loop--break"))
(defmacro loop (&rest body)
  "Keep evaluating BODY until `end-loop' is called"
  (declare (indent 0))
  `(catch loop--break-symbol
     (while t
       ,@body)))
(defsubst end-loop (&optional result)
  "End the nearest enclosing `loop' loop"
  (throw loop--break-symbol result))

(defmacro do-while (&rest body)
  "Keep evaluating BODY until it evaluates to nil"
  (declare (indent 0))
  `(while (progn ,@body)))

;; ════════════════════════════════════════
;; highlighting

(defun my-add-face-overlay (start end face)
  (let ((overlay (make-overlay start end (current-buffer))))
    ;; TODO: Use `defface'
    (overlay-put overlay 'face face)
    (overlay-put overlay 'evaporate t)
    overlay))

;; TODO: use `defface'
(defvar my-highlight-face '(:foreground "white" :background "black"))
(defun my-highlight-region (start end)
  (interactive "r")
  (let ((overlay (my-add-face-overlay start end my-highlight-face)))
    (overlay-put overlay :my-highlight t)))

(defun my-unhighlight-all nil
  (interactive)
  (remove-overlays (point-min) (point-max) :my-highlight t))

(defvar my-highlight-faces-alist
  '((?r . (:foreground "white" :background "red2"))))
(defun my-highlight-from-char (start end)
  (interactive "r")
  (let* ((char (read-char))
         (face (cdr (assoc char my-highlight-faces-alist)))
         (my-highlight-face face))
    (if (= char ?u) (my-unhighlight-all)
      (if face (my-highlight-region start end)
        (message "No face corresponding to '%c'" char)))))
(define-key global-map (kbd "C-x r h") 'my-highlight-from-char)

;;════════════════════════════════════════
;; this is where `forest' used to be. Now its in forest.el
(require 'forest)

;;════════════════════════════════════════
;; misc

(defun my-maplines (fun beg end)
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

(defun my-jump-to-marker (marker)
  (unless (marker-buffer marker)
    (error "Cannot jump to a marker whose buffer is nil"))
  (unless (marker-position marker)
    (error "Cannot jump to a marker whose position is nil"))
  (switch-to-buffer (marker-buffer marker))
  (goto-char marker))

(defun my-same-line (pos1 pos2)
  (save-excursion
    (= (progn (goto-char pos1) (line-beginning-position))
       (progn (goto-char pos2) (line-beginning-position)))))(defun my-buffer-overlay-substring (start end)
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

(defvar my-blank-line-re "^[ \t]*$"
  "Regexp which matches a blank line")

(defsubst ensure-newline nil
  "Useful when point is at the end of the buffer"
  (unless (= (char-before) ?\n)
    (insert-char ?\n)))

(defun my-zip-alist (list1 list2)
  "Create an alist whose keys are LIST1 and whose values are LIST2, respectively"
  (let ((result nil))
    (while (and list1 list2)
      (push (cons (car list1) (car list2)) result)
      (setq list1 (cdr list1) list2 (cdr list2)))
    (reverse result)))

;;════════════════════════════════════════
(provide 'my-macs)
