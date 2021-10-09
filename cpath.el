(require 'my-macs)
;;----------------------------------------
;; variables

(defvar cpath-trees nil
  "A list of the top-level call trees")
(defvar cpath-current-node nil
  "A reference to the node representing the current call on the stack")
(defvar-local cpath-func-name-func nil
  "The function used to get the name of the function at point.
It doesn't accept any arguments and returns a string. If a name cannot be
extracted, an error should be signaled")

;;----------------------------------------
;; Nodes of the call tree. A node is represented as a plist with properties
;; (:parent :children :marker :name)

(defun cpath-node (&rest args)
  "Create a node.
ARGS should be a plist with keys (:parent :children :marker :name). When :parent
is non-nil, inserts the node in the children of the parent."
  (my-plist-foreach
   (lambda (key value)
     (unless (memq key '(:parent :children :marker :name))
       (error "Bad argument: %s" key)))
   args)
  (let ((node args)
        (parent (plist-get args :parent))
        children)
    (when parent
      ;; add as a child
      (if (setq children (plist-get parent :children))
          (nconc children (list node))
        (plist-put parent :children (list node)))
      (cpath-node-set-branch parent node))
    node))

(defsubst cpath-node-delete-child (parent child)
  (let ((children (plist-get parent :children)))
    (plist-put parent :children (delq child children))))

(defun cpath-node-set-branch (parent child)
  "Sets up CHILD as PARENT's next step in its branch.
Only one child of a node is marked as the next one in the current branch. A
branch is then a path from the root where each internal node is so marked."
  (unless (memq child (plist-get parent :children))
    (error "CHILD is not a child of PARENT"))
  (dolist (child (plist-get parent :children))
    (plist-put child :branch nil))
  (plist-put child :branch t))

(defun cpath-node-branch-child (node)
  "Return the child which defines the current branch of NODE"
  (car (seq-drop-while (lambda (node)
                         (not (plist-get node :branch)))
                       (plist-get node :children))))

;;----------------------------------------
;; misc

(defsubst cpath--check-current nil
  "Raises an error when a call path is not active"
  (unless cpath-current-node
    (error "Not on a call path")))

(defsubst cpath--current-root nil
  "Return the root of the current call path
Assumes a call path is active"
  (let ((current cpath-current-node)
        parent)
    (while (setq parent (plist-get current :parent))
      (setq current parent))
    current))

(defun cpath-org-tree nil
  "Return a string representing the current call tree in the org format
Assumes a call path is active"
  (with-temp-buffer
    (cpath-depth-first-walk
     (lambda (node depth)
       (insert (make-string (1+ depth) ?*) " "
               (plist-get node :name) "\n"))
     (cpath--current-root))
    (buffer-string)))

(defun cpath-depth-first-walk (func root)
  "Traverse in depth-first order the subtree whose root is ROOT and call FUNC on
each node. FUNC is called on a parent before it is called on a child. FUNC
accepts two arguments: the current node and its depth from the root. The root
itself has depth zero, its children have depth one, etc."
  (let (;; each stack element is a pair (NODES . DEPTH)
        ;; an assumption is that NODES is never nil
        (stack (list (cons (list root) 0)))
        head nodes node depth children)
    (while stack
      (setq head (car stack)
            nodes (car head)
            node (car nodes)
            children (plist-get node :children)
            depth (cdr head))
      (funcall func node depth)
      (setq nodes (cdr nodes))
      (if nodes
          (setcar head nodes)
        (pop stack))
      (when children
        (push (cons children (1+ depth))
              stack)))))

(defsubst cpath--jump nil
  (my-jump-to-marker (plist-get cpath-current-node :marker)))

;;----------------------------------------
;; commands

(defun cpath-call nil
  "\"Calls\" the function at point, which technically means that a child to the
current node is created which corresponds to the function at point"
  (interactive)
  (let ((name (funcall cpath-func-name-func))
        (marker (point-marker))
        node)
    (if cpath-current-node
        (progn
          (setq node (cpath-node :parent cpath-current-node
                                 :name name
                                 :marker marker))
          (setq cpath-current-node node))
      (setq cpath-current-node
            (cpath-node :name name :marker marker)))))

(defun cpath-up nil
  "Go to and mark as current the caller of the current node"
  (interactive)
  (cpath--check-current)
  (let ((parent (plist-get cpath-current-node :parent)))
    (unless parent
      (user-error "Cannot move up, at top"))
    (setq cpath-current-node parent)
    (cpath--jump)))

(defun cpath-down nil
  (interactive)
  (cpath--check-current)
  (let ((child (cpath-node-branch-child cpath-current-node)))
    (unless child
      (user-error "Cannot move down, at bottom"))
    (setq cpath-current-node child)
    (cpath--jump)))

(defun cpath-goto-current nil
  (interactive)
  (cpath--check-current)
  (cpath--jump))

(defun cpath-prune nil
  "Deletes the current subtree and the parent becomes the new current node. When
the current node is a root, removes the whole tree and the current node becomes
the roto of the first top-level tree."
  (interactive)
  (cpath--check-current)
  (let ((current cpath-current-node)
        (parent (plist-get cpath-current-node :parent)))
    (if parent
        (progn (setq cpath-current-node parent)
               (cpath-node-delete-child parent current))
      (delq current cpath-trees)
      (setq cpath-current-node (car cpath-trees)))))

(defun cpath-save-org-tree nil
  "Stores in the kill ring a representation of the current call tree in the org format"
  (interactive)
  (cpath--check-current)
  (kill-new (cpath-org-tree)))

;;----------------------------------------
(provide 'cpath)
