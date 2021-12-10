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
      (LIST-neighbor siblings node direction))))

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
    ;; use SETQ here because PROPERTIES may be nil
    (setq node (plist-put node :parent parent))
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

(defun forest-new-parent (forest &rest properties)
  (let* ((node (copy-sequence properties))
         (current (forest-current forest))
         (current-parent (plist-get current :parent)))
    (plist-put current :parent node)
    (plist-put node :children (list current))
    (plist-put current-parent :children
      (--map-when (eq it current) node (plist-get current-parent :children)))
    (plist-put node :parent current-parent)
    (forest-set-current forest node)))

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
            list-add-func (cond ((eq direction :prev) 'LIST-add-before)
                                ((eq direction :next) 'LIST-add-after)
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
  (setq cursor-type nil)
  (make-local-variable 'post-command-hook)
  (add-hook 'post-command-hook
            (lambda nil
              (let ((branch-child (forest-branch-child
                                   (forest-current forest-select-forest))))
                (when branch-child (forest-draw-goto-node branch-child))))))

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
            new-branch-child (LIST-neighbor children branch-child direction))
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
         (new-root (LIST-neighbor
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

;;════════════════════
;; forest-misc

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
;; forest-misc-dir-trees

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
(provide 'forest)
