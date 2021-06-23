(defun shuffled-number-sequence (low high)
  ;; not very efficient but then again I'm not sure I will use it that much, so
  ;; I won't bother making it more efficient right now. Also, this function is
  ;; kind of flawed. Consider having (1 2) as arguments. The probability that
  ;; they will be swapped is 1/4, whereas it should be 1/2
  (if (> low high)
      (error "LOW (%s) must be less than or equal to HIGH (%s)" low high))
  (let* ((vec (vector-number-sequence low high))
         (size (length vec))
         index1 index2 val1 val2)
    (dotimes (_ (length vec))
      (setq index1 (random size) index2 (random size)
            val1 (aref vec index1) val2 (aref vec index2))
      (aset vec index1 val2) (aset vec index2 val1))
    (append vec nil)))

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
