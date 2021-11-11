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

(defun my-list-neighbor (list elt direction &optional different)
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
             (car (my-list-prev-cons
                   list (lambda (other-elt) (eq elt other-elt))))))
          
          ((eq direction :next)
           (let ((tail (memq elt list)))
             (when tail
               (if (cdr tail) (cadr tail) (car list)))))
          (t (error "Invalid direction: %s" direction)))))

(defun my-list-add-after (list elt new)
  "Returns a list formed by adding NEW after ELT. Modifies LIST"
  (catch :break
    (my-loop-cons (cell list)
      (when (eq (car cell) elt)
        (setcdr cell (cons new (cdr cell)))
        (throw :break nil))))
  list)

(defun my-list-add-before (list elt new)
  "Returns a list formed by adding NEW before ELT. Modifies LIST (unless ELT is
the head of LIST)"
  (let ((cell (my-list-prev-cons list (lambda (x) (eq x elt)))))
    (if (not cell)
        (cons new list)
      (setcdr cell (cons new (cdr cell)))
      list)))

(defun my-list-add (list elt new direction)
  "Return a list formed by adding NEW before (when DIRECTION is :prev) or after
(when DIRECTION is :next) ELT."
  (cond ((eq direction :next) (my-list-add-after list elt new))
        ((eq direction :prev) (my-list-add-before list elt new))
        (t (error "Invalid direction: %s" direction))))

(defun my-list-remove (list-sym pred)
  "Remove from the list stored at LIST-SYM the first element which satisifes PRED.
Returns t when something was actually removed and nil otherwise."
  (let ((list (symbol-value list-sym)) (prev-cons nil))
    (when list
      (if (funcall pred (car list))
          (progn (set list-sym (cdr list)) t)
        (setq prev-cons (my-list-prev-cons list pred))
        (when prev-cons (setcdr prev-cons (cddr prev-cons)) t)))))

(defun my-list-delete-and-tell (list pred)
  (let (prev-cons)
    (when list
      (if (funcall pred (car list))
          (cons (cdr list) t)
        (setq prev-cons (my-list-prev-cons list pred))
        (if prev-cons
          (progn (setcdr prev-cons (cddr prev-cons))
                 (cons list t))
          (cons list nil))))))
  

;; ════════════════════════════════════════
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
;; highlight

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

;; ════════════════════════════════════════
;; forest

(defvar forest-marker (make-symbol "forest-marker"))
(defun forest nil
  "Return an empty forest"
  (list :type forest-marker :current nil :children nil))

;;════════════════════
;; forest-misc

(defsubst forest-root-p (node)
  (eq (plist-get (plist-get node :parent) :type) forest-marker))

(defun forest-root (node)
  "Return the root of the tree of NODE. This is defined to be the ancestor which
doesn't have a parent"
  (while (not forest-root-p node)
    (setq node (plist-get node :parent))))

(defsubst forest-set-current (forest node)
  "Set NODE as the current of FOREST and return NODE, and also make NODE the
branch child of its parent."
  (plist-put forest :current node)
  (when node (forest-set-branch-child node))
  node)

(defsubst forest-current (forest)
  "Return FOREST's current node, or nil when there is no current"
  (plist-get forest :current))

(defsubst forest-empty-p (forest)
  (null (plist-get forest :children)))

(defun forest-depth-first-walk (func root &optional postorder)
  "Traverse in depth-first order the subtree whose root is ROOT and call FUNC on
each node. The traversal is in pre-order (FUNC is called on a parent called
before any of its children), unless POSTORDER is non-nil. FUNC accepts two
arguments: the current node and its depth from the root. The root itself has
depth zero, its children have depth one, etc."
  (let* (;; Each stack element corresponds to a node in the tree and has the
         ;; form [NODE CHILDREN DEPTH]
         (stack (list (vector root (plist-get root :children) 0)))
         head node children child prune-p)
    (while stack
      (setq head (car stack) node (aref head 0)
            children (aref head 1) depth (aref head 2))
      (if postorder
          (if children
              (progn (setq child (pop children))
                     (aset head 1 children)
                     (push (vector child
                                   (plist-get child :children)
                                   (1+ depth))
                           stack))
            (funcall func node depth)
            (pop stack))
        (if node
            (progn (setq prune-p t)
                   (catch 'forest-walk-prune
                     (funcall func node depth) (setq prune-p nil))
                   (if (or prune-p (not children)) (pop stack)
                     (aset head 0 nil)))
          (setq child (pop children))
          (if children (aset head 1 children) (pop stack))
          (push (vector child (plist-get child :children) (1+ depth)) stack))))))

(defun forest--check (forest)
  (when (forest-empty-p forest)
    (error "Forest is empty"))
  (unless (forest-current forest)
    (error "Forest doesn't have a current node set")))

(defun forest-branch-child (node)
  "Returns the branch child of NODE. Returns nil IFF node is a leaf"
  (let* ((children (plist-get node :children))
         result)
    (when children
      (setq result (car (seq-drop-while
                         (lambda (node) (not (plist-get node :branch-child-p)))
                         children)))
      (or result (car children)))))

(defun forest-set-branch-child (child)
  "Sets up CHILD as its parent's next step on its branch.
Only one child of a node is marked as the next one in the current branch. A
branch is then a path from the root where each internal node is so marked."
  (unless (forest-root-p child)
    (let ((parent (plist-get child :parent)))
      (dolist (child (plist-get parent :children))
        (plist-put child :branch-child-p nil))
      (plist-put child :branch-child-p t))))

(defun forest-get-sibling (node direction)
  "Return the previous or next sibling, depending on DIRECTION, which is one of
(:prev :next). When NODE is an only child, return nil."
  (let* ((siblings (plist-get (plist-get node :parent) :children)))
    (when (cdr siblings)
      (my-list-neighbor siblings node direction))))

(defun forest-in-forest-p (forest node)
  "Return t when NODE is a FOREST node"
  (let (parent)
    (while (setq parent (plist-get node :parent))
      (setq node parent))
    (eq forest node)))

(defsubst forest-current-root (forest)
  (let ((node (forest-current forest)))
    (while (not (forest-root-p node))
      (setq node (plist-get node :parent)))
    node))

;;════════════════════
;; forest-node-creation

(defun forest--new-child (parent &rest properties)
  (let ((node (copy-sequence properties)))
    (plist-put node :parent parent)
    (plist-put parent :children
      (nconc (plist-get parent :children) (list node)))
    node))

(defsubst forest-new-root (forest &rest properties)
  "Add a new root in FOREST and return the new node. The PROPERTIES must be a
property list, and the properties will be attached to the new node."
  (forest-set-current forest (apply 'forest--new-child forest properties)))

(defun forest-new-child (forest &rest properties)
  "Add a new child of the current node of FOREST. When there is no current node,
adds the node as a root. The new node becomes the current one and is returned"
  (let* ((parent (or (forest-current forest) forest)))
    (forest-set-current forest (apply 'forest--new-child parent properties))))

(defun forest-new-sibling (forest direction &rest properties)
  "Add a new sibling to the current node and make it current. When DIRECTION is
:next, the sibling is ordered after the current node, and when DIRECTION is
:prev before it. When the current node is a root, a new root is created again
ordered based on DIRECTION. When there is no current node, a new root is
created."
  (let ((current (forest-current forest))
        node parent func)
    (if (not current)
        (apply 'forest-new-root forest properties)
      (setq parent (plist-get current :parent)
            list-add-func (cond ((eq direction :prev) 'my-list-add-before)
                                ((eq direction :next) 'my-list-add-after)
                                (t (error "Invalid direction: %s" direction)))
            node (copy-sequence properties))
      (plist-put node :parent parent)
      (plist-put parent :children
        (funcall list-add-func (plist-get parent :children) current node))
      (forest-set-current forest node))))

;;════════════════════
;; node removal

(defun forest-prune (forest)
  "Remove the whole tree whose root is the current node. The new current node
will be the next sibling if there is one and otherwise the parent."
  (let* ((current (forest-current forest))
         (parent (plist-get current :parent))
         (next-current (or (forest-get-sibling current :next)
                           (and (not (forest-root-p current)) parent))))
    (plist-put parent :children
      (delq current (plist-get parent :children)))
    (plist-put current :parent nil)
    (forest-set-current forest next-current)))

(defun forest-prune-all (forest pred)
  "For all nodes which pass PRED, remove them and their subtrees"
  (let (nodes (current (forest-current forest)))
    (dolist (root (plist-get forest :children))
      (forest-depth-first-walk
       (lambda (node depth)
         (when (funcall pred node)
           (push node nodes) (throw 'forest-walk-prune nil)))
       root))
    (dolist (node nodes)
      (forest-set-current forest node) (forest-prune forest))
    (unless (forest-in-forest-p forest current)
      (forest-set-current forest (car (plist-get forest :children))))))
(put 'forest-prune-all 'lisp-indent-function 1)
      
;;════════════════════
;; navigation

(defun forest-goto-parent (forest)
  "Set the current node as the parent of the current one. Signals an error when
the current is a root."
  (forest--check forest)
  (let* ((current (forest-current forest))
         (parent (plist-get current :parent)))
    (when (forest-root-p current)
      (error "Cannot go up, on root"))
    (forest-set-current forest parent)))

(defun forest-goto-child (forest)
  "Set the current node as the branch child of the current one. Signals an error when
the current is a leaf."  
  (forest--check forest)
  (let* ((current (forest-current forest))
         (branch-child (forest-branch-child current)))
    (unless branch-child
      (error "Cannot go down, on a leaf"))
    (forest-set-current forest branch-child)))

(defun forest-goto-sibling (forest direction)
  "Set the current node as the next sibling (when DIRECTION is :next) or the
previous one (when DIRECTION is :prev) of the current one. Signals an error when
the current is an only child. The next sibling of the last child is the first
child. When on a root, this returns the next root."
  (let* ((sibling (forest-get-sibling (forest-current forest) direction)))
    (unless sibling
      (error "Cannot go to sibling, the current node is an only child"))
    (forest-set-current forest sibling)))
  
;;════════════════════════════════════════
;; forest-draw

(defvar forest-draw--node-property (make-symbol ":node")
  "Used as the text property which associates the visualization of a node with
the node object.")

(defvar-local forest-name-func 'forest-name-func-default
  "A function which accepts a node and outputs the name of the node.  The name
is going to be used as the text representing the node in the visualization. In
the visualization text the name is also going to become associated with the node
via the `treevis--node-property' property. The default function sees a node as a
plist with a :name property")
(defun forest-name-func-default (node)
  (plist-get node :name))

;;════════════════════════════════════════
;; draw

(defun forest-draw-tree (tree)
  "Make sure to call on an empty line"
  (forest-draw-node tree))

(defun forest-draw-node (node)
  "Draw NODE at point and leave point at the end of the drawing"
  (let ((children-markers nil)
        (children (plist-get node :children))
        child-start child-end connector node-string name)
    (if (not children)
        (progn (setq child-start (point))
               (insert (funcall forest-name-func node) "\n")
               (put-text-property
                child-start (1+ child-start) forest-draw--node-property node))
      (save-restriction
        (narrow-to-region (point) (point))
        (dolist (child children)
          (push (point-marker) children-markers)
          (forest-draw-node child))
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
        (my-maplines
            (lambda nil
              (when (= (char-after) ?\s)
                (delete-char 1) (insert "║")))
            (point-min) (car (last children-markers)))
        ;; insert NODE's text and associate the text with NODE through a text property
        (setq node-string (concat (funcall forest-name-func node) "═╗"))
        (indent-rigidly (point-min) (point-max) (1- (length node-string)))
        (beginning-of-buffer)
        (setq child-start (point)) (insert node-string "\n")
        (put-text-property
         child-start (1+ child-start) forest-draw--node-property node)
        (goto-char (point-max))))))

(defun forest-draw-goto-node (node)
  (beginning-of-buffer)
  (text-property-search-forward
   forest-draw--node-property node 'eq)
  (forward-char -1))

(defun forest-draw-node-at-point nil
  "Returns the node corresponding to the current line"
  (save-excursion
    (beginning-of-line)
    (text-property-search-forward forest-draw--node-property)
    (get-text-property
     (1- (point)) forest-draw--node-property)))

;;════════════════════════════════════════
;; marking

(defun forest-draw-mark-branch (leaf &optional face)
  "Marks the branch defined by the node object LEAF.
Assumes that the current buffer displays a tree which features LEAF (i.e. that
there is text associated with LEAF through the `forest-draw--node-property' text
property. Naturally, it also assumes that LEAF is actually a leaf node"
  (save-excursion
    (forest-draw-goto-node leaf)
    (forward-char (length (funcall forest-name-func leaf)))
    (let ((forest-draw-mark-face (or face forest-draw-mark-face)))
      (while (forest-draw-mark--row)
        (forest-draw-mark--column)))))

(defun forest-draw-mark-node (node &optional face)
  "Fontifies the text of the node object NODE with FACE
Assumes that the current buffer has a `treevis' tree with a text node
associated with NODE"
  (save-excursion
    (let ((forest-draw-mark-face (or face forest-draw-mark-face)))
      (forest-draw-goto-node node)
      (forest-draw--mark
       (point) (+ (point) (length (funcall forest-name-func node)))))))

(defun forest-draw-mark--column nil
  "Assumes that point is on an already marked ╚ connector. Marks the column of
the connector (i.e. up to and including the first ╗ connector)"
  (let ((column (current-column)))
  (while (not (= (char-after) ?╗))
    (beginning-of-line 0) (forward-char column)
    (forest-draw--mark (point) (1+ (point))))))

(defun forest-draw-mark--row nil
  "Marks from point up to and including the first ╚ or ╠ connector and returns
t. Returns nil when there is no such connector (which means that the root has
been marked) or when the root line has been marked."
  (let ((end (point)))
    (if (re-search-backward "╚\\|╠" (line-beginning-position) t)
        (progn (forest-draw--mark (point) end) t)
      (beginning-of-line)
      (forest-draw--mark (point) end)
      nil)))

;; TODO: Use `defface'
(defvar forest-draw-mark-face '(:foreground "white" :background "black"))
(defun forest-draw--mark (start end)
  (remove-overlays start end :forest-draw-mark t)
  (let ((overlay (my-add-face-overlay start end forest-draw-mark-face)))
    (overlay-put overlay :forest-draw-mark t)))
(defun forest-draw-unmark nil
  (remove-overlays (point-min) (point-max) :forest-draw-mark t))

;;════════════════════════════════════════
;; forest-select

(defvar forest-select--buffer-name "*forest-select*")
(defvar-local forest-select-branch-children nil
  "An alist which maps a node to its branch child. If a node does not appear,
its default branch child is the first child")
;; TODO: Use `defface'
(defvar forest-select-branch-face '(:foreground "black" :weight bold)
  "The face of the headings corresponding to the nodes of the current branch")
(defvar forest-select-current-face '(:foreground "white" :background "black")
  "The face of the heading corresponding to the current node")

(define-derived-mode forest-select-mode special-mode "Select-Node"
  (setq cursor-type nil))

(defvar-local forest-select-forest nil)
(defvar-local forest-select-initial-current nil)
(defvar-local forest-select-did-quit nil)
(defun forest-select (forest)
  "Show a buffer which visualizes FOREST. Returns t when the user didn't quit
and nil if they did quit."
  (when forest
    (let ((selection-buffer (get-buffer-create forest-select--buffer-name))
          result)
      (switch-to-buffer-other-window selection-buffer)
      (forest-select-mode)
      (setq forest-select-forest forest
            forest-select-initial-current (forest-current forest)
            forest-select-did-quit nil)
      (forest-select-redraw)
      (recursive-edit)
      (when forest-select-did-quit
        (forest-set-current
         forest-select-forest
         (if (forest-in-forest-p
              forest-select-forest forest-select-initial-current)
             forest-select-initial-current
           (car (plist-get forest :children)))))
      (setq result (not forest-select-did-quit))
      ;; killing the buffer must come after computing the result as otherwise
      ;; local variables will be lost
      (kill-buffer-and-window)
      result)))

(defun forest-select-redraw nil
  (forest-select-draw-forest forest-select-forest)
  (forest-select-mark-branch))

(defsubst forest-select-draw-forest (forest)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (tree (plist-get forest :children))
      (forest-draw-tree tree) (insert "\n"))
    (put-text-property (point-min) (point-max) 'face 'shadow)))

(defun forest-select-mark-branch nil
  (forest-draw-unmark)
  ;; mark the current branch
  (let* ((current (forest-current forest-select-forest))
         (node current) branch-child)
    ;; get the branch leaf
    (while (setq branch-child (forest-branch-child node))
      (setq node branch-child))
    (forest-draw-mark-branch node forest-select-branch-face)
    (forest-draw-mark-node current forest-select-current-face)
    (forest-draw-goto-node current)))

(defun forest-select-up nil
  (interactive)
  (forest-goto-parent forest-select-forest)
  (forest-select-mark-branch))

(defun forest-select-down nil
  (interactive)
  (forest-goto-child forest-select-forest)
  (forest-select-mark-branch))

(defun forest-select--change-branch (direction)
  "When DIRECTION is :next, set the branch child to the one following the
current one. When DIRECTION is :prev, set the branch child to the one preceding
the current one."
  (let* ((current (forest-current forest-select-forest))
         (children (plist-get current :children))
         branch-child new-branch-child)
    (unless (or (null children) (null (cdr children)))
      (setq branch-child (forest-branch-child current)
            new-branch-child (my-list-neighbor children branch-child direction))
      (forest-set-branch-child new-branch-child)
      (forest-select-mark-branch))))

(defun forest-select-branch-next nil
  "Set the branch child to the one preceding the current one"
  (interactive)
  (forest-select--change-branch :next))

(defun forest-select-branch-prev nil
  "Set the branch child to the one preceding the current one"
  (interactive)
  (forest-select--change-branch :prev))

(defun forest-select--change-root (direction)
  (let* ((current-root (forest-current-root forest-select-forest))
         (new-root (my-list-neighbor
                     (plist-get forest-select-forest :children)
                     current-root direction)))
    (unless (eq current-root new-root)
      (forest-set-current forest-select-forest new-root))
    (forest-select-mark-branch)))

(defun forest-select-next-tree nil
  (interactive)
  (forest-select--change-root :next))

(defun forest-select-prev-tree nil
  (interactive)
  (forest-select--change-root :prev))

(defun forest-select-exit nil
  (interactive)
  (exit-recursive-edit))

(defun forest-select-quit nil
  (interactive)
  (setq forest-select-did-quit t)
  (exit-recursive-edit))

(defun forest-select-prune nil
  (interactive)
  (forest-prune forest-select-forest)
  (forest-select-redraw))

(defun forest-select-list-item (name items &optional default)
  "NAME is the name of the list. ITEMS must be a list of string or symbols (the
return value is always a string, even when a symbol is selected, in which case
its name is returned). DEFAULT is optional and must be in ITEMS. It will be
highlighted as the default, but that doesn't mean it is the default return value"
  (let* ((forest (forest-from-sexp (cons name items)))
         (selected-p nil))
    (when default
      (unless (member default items)
        (error "DEFAULT is not present in ITEMS"))
      (forest-goto-child forest)
      (while (not (string= (plist-get (forest-current forest) :name) default))
        (forest-goto-sibling forest :next))
      (forest-goto-parent forest))
    (setq selected-p (forest-select forest))
    (when (and selected-p (not (forest-root-p (forest-current forest))))
      (plist-get (forest-current forest) :name))))

;;════════════════════

(defvar forest-select-mode-map (make-sparse-keymap))
(progn
  (define-key forest-select-mode-map "p" 'forest-select-up)
  (define-key forest-select-mode-map "n" 'forest-select-down)
  (define-key forest-select-mode-map "f" 'forest-select-branch-next)
  (define-key forest-select-mode-map "b" 'forest-select-branch-prev)
  (define-key forest-select-mode-map "q" 'forest-select-quit)
  (define-key forest-select-mode-map "k" 'forest-select-prune)
  (define-key forest-select-mode-map "\C-p" 'forest-select-prev-tree)
  (define-key forest-select-mode-map "\C-n" 'forest-select-next-tree)
  (define-key forest-select-mode-map (kbd "RET") 'forest-select-exit))

;;════════════════════════════════════════
;; miscellaneous

(defun forest-draw-from-org nil
  "Create a plist tree from the org tree at point"
  (let* ((entries (forest-draw--org-entries))
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

(defun forest-draw--org-entries nil
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

(defun forest-draw-read-AST nil
  (forest-draw-AST (save-excursion (read (current-buffer)))))

(defun forest-draw-AST (form)
  (cond ((consp form)
         (list :name "()"
               :children (mapcar 'forest-draw-from-sexp form)))
        ((symbolp form) (list :name (symbol-name form) :children nil))
        (t (list :name (concat "["
                               (symbol-name (type-of form))
                               " "
                               (prin1-to-string form)
                               "]")
                 :children nil))))

(defun forest-from-sexp (sexp)
  (let* ((tree (forest--sexp-to-tree sexp))
         (forest (list :type forest-marker :current nil :children (list tree))))
    (plist-put tree :parent forest)
    (forest-set-current forest tree)
    forest))

(defun forest--sexp-to-tree (sexp)
  "Assumes SEXP is an S-Expression where the only leaves are symbols.
Each list begins either with a symbol or with a string."
  (cond ((consp sexp)
         (let ((node (list :name nil :parent nil :children nil))
               (children (mapcar 'forest--sexp-to-tree (cdr sexp))))
           (plist-put node :name (if (symbolp (car sexp))
                                     (symbol-name (car sexp))
                                   (car sexp)))
           (mapc (lambda (child) (plist-put child :parent node))
                 children)
           (plist-put node :children children)
           node))
        ((symbolp sexp) (list :name (symbol-name sexp) :children nil))
        ((stringp sexp) (list :name sexp :children nil))
        (t (error "Only symbols and strings allowed, but found %S" sexp))))

;; ════════════════════
;; visualizing directory trees

(defun forest-draw-dir (path)
  (let ((forest-draw-children-func 'forest-draw-dirnode-children)
        (forest-name-func 'forest-draw-dirnode-name))
    (forest-draw-tree path)))
                   
(defun forest-draw-dirnode-name (path)
  (if (file-directory-p path)
      (file-name-nondirectory (directory-file-name path))
    (file-name-nondirectory path)))

(defun forest-draw-dirnode-children (path)
  (when (file-directory-p path)
    (seq-filter
     (lambda (file)
       (not (string-match "/\\.$\\|/\\.\\.$" file)))
     (directory-files path t))))

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

;;════════════════════════════════════════
(provide 'my-macs)
