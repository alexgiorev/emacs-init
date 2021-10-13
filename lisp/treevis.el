(require 'my-macs)
;;########################################
(defvar treevis--node-property (make-symbol ":node")
  "Used as the text property which associates the visualization of the node with
the node object.")

(defvar-local treevis-children-func 'treevis-children-func-default
  "A function which accepts a node and outputs the list of children nodes. The
default function sees a node as a plist with a :children property")

(defvar-local treevis-name-func 'treevis-name-func-default
  "A function which accepts a node and outputs the name of the node.  The name
is going to be used as the text representing the node in the visualization. In
the visualization text the name is also going to become associated with the node
via the `treevis--node-property' property. The default function sees a node as a
plist with a :name property")

(defun treevis-children-func-default (node)
  (plist-get node :children))

(defun treevis-name-func-default (node)
  (plist-get node :name))


;;########################################
;; draw
(defvar treevis-brushes
  '(("double1" :chars "╚═╗╠║" :width 1)
    ("light" :chars "└─┐├│" :width 1)
    ("heavy1" :chars "┕━┒┠┃" :width 2)
    ("heavy2" :chars "┗━┓┣┃" :width 2)
    ("double2" :chars "╘═╕╞│" :width 1)))
(defvar treevis-brush (cdr (assoc "heavy2" treevis-brushes)))
(defsubst treevis--up-right nil
  (char-to-string (aref (plist-get treevis-brush :chars) 0)))
(defsubst treevis--horizontal nil
  (char-to-string (aref (plist-get treevis-brush :chars) 1)))
(defsubst treevis--down-left nil
  (char-to-string (aref (plist-get treevis-brush :chars) 2)))
(defsubst treevis--vertical-right nil
  (char-to-string (aref (plist-get treevis-brush :chars) 3)))
(defsubst treevis--vertical nil
  (char-to-string (aref (plist-get treevis-brush :chars) 4)))
(defsubst treevis--brush-width nil
  (plist-get treevis-brush :width))
  
(defun treevis-draw (tree)
  "Make sure to call on an empty line"
  (treevis-draw-node tree))

(defun treevis-draw-node (node)
  "Draw NODE at point and leave point at the end of the drawing"
  (let ((children-markers nil)
        (children (funcall treevis-children-func node))
        child-start child-end connector node-string name)
    (if (not children)
        (progn (setq child-start (point))
               (insert (funcall treevis-name-func node) "\n")
               (put-text-property
                child-start (1+ child-start) treevis--node-property node))
      (save-restriction
        (narrow-to-region (point) (point))
        (dolist (child children)
          (push (point-marker) children-markers)
          (treevis-draw-node child))
        (setq children-markers (reverse children-markers))
        ;; insert connectors
        (my-loop-cons (pair children-markers)
          (setq child-start (car pair))
          (if (cdr pair)
              ;; a connector equivalent to "╠═"
              (setq connector (concat (treevis--vertical-right) (treevis--horizontal))
                    child-end (cadr pair))
            ;; a connector equivalent to "╚═"
            (setq connector (concat (treevis--up-right) (treevis--horizontal))
                  child-end (point-max)))
          (goto-char child-start)
          (insert connector)
          (unless (= (progn (beginning-of-line 2) (point)) child-end)
            (indent-rigidly (point) child-end (* 2 (treevis--brush-width)))))
        ;; connect connectors
        (my-maplines (point-min) (car (last children-markers))
          (lambda nil
            (when (= (char-after) ?\s)
              (delete-char (treevis--brush-width)) (insert (treevis--vertical)))))
        ;; insert NODE's text and associate the text with NODE through a text property
        (setq name (funcall treevis-name-func node)
              node-string (concat name (treevis--horizontal) (treevis--down-left)))
        (indent-rigidly (point-min) (point-max)
                        (+ (length name) (treevis--brush-width)))
        (beginning-of-buffer)
        (setq child-start (point)) (insert node-string "\n")
        (put-text-property
         child-start (1+ child-start) treevis--node-property node)
        (goto-char (point-max))))))

;;########################################
;; utils

(defun treevis-goto-node (node)
  (beginning-of-buffer)
  (text-property-search-forward
   treevis--node-property node 'eq)
  (forward-char -1))

(defun treevis-node-at-point nil
  "Returns the node corresponding to the current line"
  (save-excursion
    (beginning-of-line)
    (text-property-search-forward treevis--node-property)
    (get-text-property
     (1- (point)) treevis--node-property)))

;;########################################
;; marking

(defun treevis-mark-branch (leaf &optional face)
  "Marks the branch defined by the node object LEAF.
Assumes that the current buffer displays a tree which features LEAF (i.e. that
there is text associated with LEAF through the `treevis--node-property' text
property. Naturally, it also assumes that LEAF is actually a leaf node"
  (save-excursion
    (treevis-goto-node leaf)
    (forward-char (length (funcall treevis-name-func leaf)))
    (let ((treevis-mark-face (or face treevis-mark-face)))
      (while (treevis-mark--row)
        (treevis-mark--column)))))

(defun treevis-mark-node (node &optional face)
  "Fontifies the text of the node object NODE with FACE
Assumes that the current buffer has a `treevis' tree with a text node
associated with NODE"
  (save-excursion
    (let ((treevis-mark-face (or face treevis-mark-face)))
      (treevis-goto-node node)
      (treevis--mark
       (point) (+ (point) (length (funcall treevis-name-func node)))))))

(defun treevis-mark--column nil
  (let ((column (current-column)))
  (while (not (= (char-after) (string-to-char (treevis--down-left))))
    (beginning-of-line 0) (forward-char column)
    (treevis--mark (point) (1+ (point))))))

(defun treevis-mark--row nil
  (let ((end (point))
        (regexp (format "%s\\|%s" (treevis--up-right) (treevis--vertical-right))))
    (if (re-search-backward regexp (line-beginning-position) t)
        (progn (treevis--mark (point) end) t)
      (beginning-of-line)
      (treevis--mark (point) end)
      nil)))

;; TODO: Use `defface'
(defvar treevis-mark-face '(:foreground "white" :background "black"))
(defun treevis--mark (start end)
  (remove-overlays start end :treevis-mark t)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face treevis-mark-face)
    (overlay-put overlay :treevis-mark t)
    (overlay-put overlay 'evaporate t)))
(defun treevis-unmark nil
  (remove-overlays (point-min) (point-max) :treevis-mark t))

;;########################################
;; fun utilities

(defun treevis-from-org nil
  "Create a plist tree from the org tree at point"
  (let* ((entries (treevis--org-entries))
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
                       (append (funcall treevis-children-func node)
                               (list (cdar entries2)))))
          ;; continue with next entry
          (pop entries2))))
    root))

(defun treevis--org-entries nil
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

(defun treevis-from-sexp (form)
  (cond ((consp form)
         (list :name "()"
               :children (mapcar 'treevis-from-sexp form)))
        ((symbolp form) (list :name (symbol-name form) :children nil))
        (t (list :name (concat "["
                               (symbol-name (type-of form))
                               " "
                               (prin1-to-string form)
                               "]")
                 :children nil))))

;; visualizing directory trees
(defun treevis-draw-dir (path)
  (let ((treevis-children-func 'treevis-dirnode-children)
        (treevis-name-func 'treevis-dirnode-name))
    (treevis-draw path)))
                   
(defun treevis-dirnode-name (path)
  (if (file-directory-p path)
      (file-name-nondirectory (directory-file-name path))
    (file-name-nondirectory path)))

(defun treevis-dirnode-children (path)
  (when (file-directory-p path)
    (seq-filter
     (lambda (file)
       (not (string-match "/\\.$\\|/\\.\\.$" file)))
     (directory-files path t))))
