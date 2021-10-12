(defvar my-treevis-node-property (make-symbol ":node")
  "Used as the text property which associates the visualization of the node with
the node object.")

(defun my-treevis-draw (&optional root)
  "Make sure to call on an empty line"
  (setq root (or root (cpath--current-root)))
  (insert (my-treevis-draw-node root)))

(defun my-treevis-draw-node (node)
  "Draw NODE at point and leave point at the end of the drawing"
  (let ((children-markers nil)
        (children (plist-get node :children))
        child-start child-end connector node-string)
    (if (not children)
        (progn (setq child-start (point))
               (insert (plist-get node :name) "\n")
               (put-text-property
                child-start (1+ child-start) my-treevis-node-property node))
      (save-restriction
        (narrow-to-region (point) (point))
        (dolist (child children)
          (push (point-marker) children-markers)
          (my-treevis-draw-node child))
        (setq children-markers (reverse children-markers))
        ;; insert connectors
        (my-loop-cons (pair children-markers)
          (setq child-start (car pair))
          (if (cdr pair)
              (setq connector "╠═"
                    child-end (cadr pair))
            (setq connector "╚═"
                  child-end (point-max)))
          (goto-char child-start)
          (insert connector)
          (unless (= (progn (beginning-of-line 2) (point)) child-end)
            (indent-rigidly (point) child-end 2)))
        ;; connect connectors
        (my-maplines (point-min) (car (last children-markers))
          (lambda nil
            (when (= (char-after) ?\s)
              (delete-char 1) (insert "║"))))
        ;; insert NODE's text and associate the text with NODE through a text property
        (setq node-string (concat (plist-get node :name) "═╗"))
        (indent-rigidly (point-min) (point-max) (1- (length node-string)))
        (beginning-of-buffer)
        (setq child-start (point)) (insert node-string "\n")
        (put-text-property
         child-start (1+ child-start) my-treevis-node-property node)
        (goto-char (point-max))))))

(defun my-treevis-mark-branch (leaf face)
  "Marks the branch defined by the node object LEAF.
Assumes that the current buffer displays a tree which features LEAF (i.e. that
there is text associated with LEAF through the `my-treevis-node-property' text
property. Naturally, it also assumes that LEAF is actually a leaf node"
  (beginning-of-buffer)
  (text-property-search-forward my-treevis-node-property leaf t)
  (end-of-line)
  (while (my-treevis-mark--row)
    (my-treevis-mark--column)))

(defun my-treevis-mark-node (node face)
  "Fontifies the text of the node object NODE with FACE
Assumes that the current buffer has a `my-treevis' tree with a text node
associated with NODE"
  (error "TODO"))

(defun my-treevis-goto-node (node)
  (beginning-of-buffer)
  (text-property-search-forward
   my-treevis-node-property node 'eq)
  (forward-char -1))

(defun my-treevis-mark--column nil
  "Assumes that point is on an already marked ╚ connector. Marks the column of
the connector (i.e. up to and including the first ╗ connector)"
  (let ((column (current-column)))
  (while (not (= (char-after) ?╗))
    (beginning-of-line 0) (forward-char column)
    (my-treevis--mark (point) (1+ (point))))))

(defun my-treevis-mark--row nil
  "Marks from point up to and including the first ╚ or ╠ connector. Returns nil when
there is no such connector (which means that the root has been marked), and t
otherwise."
  (let ((end (point)))
    (if (re-search-backward "╚\\|╠" nil t)
        (progn (my-treevis--mark (point) end) t)
      (beginning-of-line)
      (my-treevis--mark (point) end)
      nil)))

;; TODO: Use `defface'
(defvar my-treevis-mark-face nil)
(setq my-treevis-mark-face '(:foreground "white" :background "black"))
(defun my-treevis--mark (start end)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face my-treevis-mark-face)
    (overlay-put overlay :my-treevis-mark t)
    (overlay-put overlay 'evaporate t)))
(defun my-treevis--unmark nil
  (remove-overlays (point-min) (point-max) :my-treevis-mark t))

(defun my-treevis--org-entries nil
  "Returns a list of pairs of the form (DEPTH . NODE)"
  (let (entries (root-level (funcall outline-level)))
    (org-map-tree
     (lambda nil
       (push
        (cons (- (funcall outline-level) root-level)
              (list :name (org-no-properties (org-get-heading t t t t))
                    :children nil))
        entries)))
    (reverse entries)))

(defun my-treevis-from-org nil
  (let* ((entries (my-treevis--org-entries))
         (root (cdar entries)))
    ;; at the end of this loop, ROOT will be the root and all parent/child
    ;; relationships between the nodes will be formed
    (while entries
      (let* ((entry (pop entries))
             (depth (car entry)) (node (cdr entry))
             (entries2 entries))
        (while (and entries2 (< depth (caar entries2)))
          ;; if we are positioned on a child of NODE...
          (when (= (caar entries2) (1+ depth))
            (plist-put node :children
                       (append (plist-get node :children)
                               (list (cdar entries2)))))
          ;; continue with next entry
          (pop entries2))))
    root))

(defun my-treevis-remove-names (tree)
  (cpath-depth-first-walk
   (lambda (node depth)
     (plist-put node :name "X"))
   tree)
  tree)

(defun my-treevis-from-sexp (form)
  (cond ((consp form)
         (list :name "()"
               :children (mapcar 'my-treevis-from-sexp form)))
        ((symbolp form) (list :name (symbol-name form) :children nil))
        (t (list :name (concat "[" (symbol-name (type-of form)) "]") :children nil))))
