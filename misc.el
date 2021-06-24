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

(defun my/touch-left (beg end)
  "Shifts lines in [BEG, END] to the left until the some line is no longer indented."
  (interactive "r")
  (let ((mini (my/min-indentation beg end)))
    (my/maplines beg end
                  (lambda () (my/reduce-indentation mini)))))

(defun my/min-indentation (beg end)
  "Return the minimum indentation among the lines delimited by BEG and END.
Ignores blank lines."
  (let ((result 1000))
    (my/maplines
     beg end
     (lambda ()
       (unless (looking-at "^[[:blank:]]*$")
         (setq result (min result (current-indentation))))))
    result))

(defun my/reduce-indentation (amount)
  "Reduce the indentation of the current line by AMOUNT.
Assumes that point is at the beginning of the line."
  (let ((start (point)))
    (skip-chars-forward "[[:blank:]]" (+ start amount))
    (delete-region start (point))))

(defun my/maplines (beg end fun)
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
