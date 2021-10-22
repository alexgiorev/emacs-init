(require 'my-macs)
;;########################################
;; variables

(defvar cpath-trees nil
  "A list of the top-level call trees")
(defvar cpath-current-node nil
  "A reference to the node representing the current call on the stack")
(defvar-local cpath-func-name-func nil
  "The function used to get the name of the function at point.
It doesn't accept any arguments and returns a string. If a name cannot be
extracted, an error should be signaled")

;;########################################
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
      (cpath-node-set-branch-child parent node))
    node))

(defsubst cpath-node-delete-child (parent child)
  (let ((children (plist-get parent :children)))
    (plist-put parent :children (delq child children))))

(defun cpath-node-set-branch-child (parent child)
  "Sets up CHILD as PARENT's next step in its branch.
Only one child of a node is marked as the next one in the current branch. A
branch is then a path from the root where each internal node is so marked."
  (unless (memq child (plist-get parent :children))
    (error "CHILD is not a child of PARENT"))
  (dolist (child (plist-get parent :children))
    (plist-put child :branch nil))
  (plist-put child :branch t))

(defun cpath-node-get-branch-child (node)
  "Returns the child which defines the current branch of NODE. Returns nil when
NODE is a leaf."
  (let* ((children (plist-get node :children))
         result)
    (when children
      (setq result (car (seq-drop-while
                         (lambda (node) (not (plist-get node :branch)))
                         children)))
      (or result (progn (cpath-node-set-branch-child node (car children))
                        (car children))))))

(defsubst cpath-node-leafp (node)
  (null (plist-get node :children)))

;;########################################
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

(defsubst cpath--jump nil
  (my-jump-to-marker (plist-get cpath-current-node :marker)))

;;########################################
;; commands

(defun cpath-call (&optional arg)
  "\"Calls\" the function at point, which technically means that a child to the
current node is created which corresponds to the function at point. When called
with a prefix argument, makes a top-level call."
  (interactive "P")
  (let ((name (funcall cpath-func-name-func))
        (marker (point-marker))
        (rootp (or arg (not cpath-current-node)))
        node)
    (if (not rootp)
        (progn
          (setq node (cpath-node :parent cpath-current-node
                                 :name name
                                 :marker marker))
          (setq cpath-current-node node))
      (setq cpath-current-node
            (cpath-node :name name :marker marker))
      (add-to-list 'cpath-trees cpath-current-node :append)))
  (message "Called %s" (plist-get cpath-current-node :name)))

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
  (let ((child (cpath-node-get-branch-child cpath-current-node))
        (children (plist-get cpath-current-node :children)))
    (unless children
      (user-error "Cannot move down, at bottom"))
    (unless child
      (user-error "Cannot move down, no branch child"))
    (setq cpath-current-node child)
    (cpath--jump)))

(defun cpath-goto-current nil
  (interactive)
  (cpath--check-current)
  (cpath--jump))

(defun cpath-prune nil
  "Deletes the current subtree and the parent becomes the new current node. When
the current node is a root, removes the whole tree and the current node becomes
the root of the first top-level tree."
  (interactive)
  (cpath--check-current)
  (let ((current cpath-current-node)
        (parent (plist-get cpath-current-node :parent)))
    (cpath--prune cpath-current-node)
    (setq cpath-current-node (or parent (car cpath-trees)))
    (message "Pruned subtree of \"%s\"" (plist-get current :name))))

(defun cpath--prune (node)
  "Remove the tree whose root is NODE. Make sure to adjust `cpath-current-node'
if it was one of the nodes removed."
  (let* ((parent (plist-get node :parent)))
    (if parent
        (progn (plist-put parent :children
                 (delq node (plist-get parent :children))))
      (setq cpath-trees (delq node cpath-trees)))
    (plist-put node :parent nil)))

(defsubst cpath-in-forest-p (node)
  "Returns non-nil when NODE is a node in the `cpath-trees' forest. This is
useful because sometimes nodes leave the forest because an ancestor was
removed."
  (not (null (memq (my-tree-root node) cpath-trees))))

;; No need to override the treevis functions for name, parent and children, as
;; they assume a plist node. Only override the `treevis-select' functions
(defun cpath-select nil
  (interactive)
  (unless cpath-trees
    (user-error "Forest is empty"))
  (let* ((treevis-select-get-branch-child-func 'cpath-node-get-branch-child)
         (treevis-select-set-branch-child-func 'cpath-node-set-branch-child)
         (treevis-select-prune-func 'cpath--prune)
         (node (treevis-select cpath-trees cpath-current-node)))
    (if node
        (progn (setq cpath-current-node node)
               (cpath--jump))
      ;; no node was selected, but the current one could have been pruned
      (unless (cpath-in-forest-p cpath-current-node)
        (setq cpath-current-node (car cpath-trees))))))

;;########################################
;; keymap
(defvar cpath-map (make-sparse-keymap))
(progn
  (define-key cpath-map "c" 'cpath-goto-current)
  (define-key cpath-map "p" 'cpath-up)
  (define-key cpath-map "n" 'cpath-down)
  (define-key cpath-map "d" 'cpath-prune)
  (define-key cpath-map " " 'cpath-call)
  (define-key cpath-map "e" 'cpath-select))
(define-key prog-mode-map "\C-cp" cpath-map)
;;########################################
(provide 'cpath)
