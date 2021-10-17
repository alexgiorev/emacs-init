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

(defun cpath-org-tree nil
  "Return a string representing the current call tree in the org format
Assumes a call path is active"
  (with-temp-buffer
    (cpath-depth-first-walk
     (lambda (node depth)
       (insert (make-string (1+ depth) ?*) " "
               (plist-get node :name) "\n")))
    (buffer-string)))

(defun cpath-depth-first-walk (func &optional root postorder)
  "Traverse in depth-first order the subtree whose root is ROOT and call FUNC on
each node. The traversal is in pre-order (parent called before children), unless
POSTORDER is non-nil. ROOT defaults to the current root, but it can be any node,
in which case the traversal is performed on the subtree of the node. FUNC
accepts two arguments: the current node and its depth from the root. FUNC is
called on a parent before it is called on a child. The root itself has depth
zero, its children have depth one, etc."
  (let* ((root (or root (cpath--current-root)))
         ;; Each stack element corresponds to a node in the tree and has the
         ;; form [NODE CHILDREN DEPTH]
         (stack (list (vector root (plist-get root :children) 0)))
         head node children child)
    (while stack
      (setq head (car stack)
            node (aref head 0)
            children (aref head 1)
            depth (aref head 2))
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
            (progn (funcall func node depth)
                   (aset head 0 nil)
                   (when (not children) (pop stack)))
          (setq child (pop children))
          (if children (aset head 1 children) (pop stack))
          (push (vector child (plist-get child :children) (1+ depth)) stack))))))
      
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
the roto of the first top-level tree."
  (interactive)
  (cpath--check-current)
  (let ((current cpath-current-node)
        (parent (plist-get cpath-current-node :parent)))
    (if parent
        (progn (setq cpath-current-node parent)
               (cpath-node-delete-child parent current))
      (setq cpath-trees (delq current cpath-trees))
      (setq cpath-current-node (car cpath-trees)))
    (message "Pruned subtree of \"%s\""
             (plist-get current :name))))

(defun cpath-save-org-tree nil
  "Stores in the kill ring a representation of the current call tree in the org format"
  (interactive)
  (cpath--check-current)
  (kill-new (cpath-org-tree)))

(defun cpath-branch nil
  "Change the branch of the current node. Shows a list of options about the next step."
  (interactive)
  (cpath--check-current)
  (let ((names (mapcar (lambda (child)
                         (if (plist-get child :branch)
                             (cons (plist-get child :name) t)
                           (plist-get child :name)))
                       (plist-get cpath-current-node :children)))
        choice child)
    (unless names (user-error "On a leaf"))
    (with-temp-buffer
      (rename-buffer "*cpath-branch*")
      (org-mode)
      (insert "1. "
              (if (consp (car names))
                  (format "*%s*" (caar names))
                (car names))
              "\n")
      (dolist (name (cdr names))
        (org-insert-item)
        (insert (if (consp name) (format "*%s*" (car name)) name) "\n"))
      (save-window-excursion
        (switch-to-buffer-other-window (current-buffer))
        (setq choice
              (if (< (length names) 10)
                  (1+ (- (read-char-exclusive "Enter number") ?1))
                (read-number "Enter number: "))))
      (when (> choice (length names))
        (user-error "%s is not a valid choice"))
      (setq child (nth (1- choice) (plist-get cpath-current-node :children)))
      (cpath-node-set-branch-child cpath-current-node child)
      (message "Set branch of current node to \"%s\"" (plist-get child :name)))))

;;--------------------
;; navigation

(defvar cpath-navigation-mode-map nil)
(let ((map (make-keymap)))
  (progn
    (define-key map "p" 'cpath-navigation-up)
    (define-key map "n" 'cpath-navigation-down)
    (define-key map "f" 'cpath-navigation-branch-next)
    (define-key map "b" 'cpath-navigation-branch-prev)
    (define-key map "q" 'cpath-navigation-quit)
    (define-key map "\C-p" 'cpath-navigation-prev-tree)
    (define-key map "\C-n" 'cpath-navigation-next-tree)
    (define-key map (kbd "RET") 'cpath-navigation-quit))
  (setq cpath-navigation-mode-map map))

;; TODO: Use `defface'
(defvar cpath-navigation-branch-face '(:foreground "black" :background "yellow")
  "The face of the headings corresponding to the nodes of the current branch")
(defvar cpath-navigation-current-face '(:foreground "white" :background "black")
  "The face of the heading corresponding to the current node")

(define-derived-mode cpath-navigation-mode special-mode "Call-Path-Navigation"
  ;;########################################
  ;; insert an org-like representation of `cpath-trees', additionally
  ;; associating via text properties each heading with an actual node
  (setq treevis-name-func (lambda (node) (plist-get node :name))
        treevis-children-func (lambda (node) (plist-get node :children)))
  (read-only-mode -1)
  (erase-buffer)
  (dolist (tree cpath-trees)
    (treevis-draw tree)
    (insert "\n"))
  (read-only-mode 1)
  ;;########################################
  (cpath-navigation--mark-branch)
  (setq cursor-type nil))

(defun cpath-navigation--mark-branch nil
  (treevis-unmark)
  ;; mark the current branch
  (let ((node (cpath--current-root)))
    (while (not (cpath-node-leafp node))
      (setq node (cpath-node-get-branch-child node)))
    (treevis-mark-branch node cpath-navigation-branch-face)
    (treevis-mark-node cpath-current-node cpath-navigation-current-face)))

(defvar cpath-navigation-buffer-name "*cpath-navigation*")
(defun cpath-navigate nil
  "Show navigation buffer"
  (interactive)
  (cpath--check-current)
  (let ((nbuffer (get-buffer-create cpath-navigation-buffer-name)))
    (switch-to-buffer-other-window nbuffer)
    (cpath-navigation-mode)
    (cpath-navigation--jump)))

(defsubst cpath-navigation--jump nil
  (select-window (previous-window))
  (cpath--jump)
  (select-window (next-window)))

(defun cpath-navigation-up nil
  (interactive)
  (let ((parent (plist-get cpath-current-node :parent)))
    (unless parent
      (user-error "Cannot move up, at root"))
    (setq cpath-current-node parent)
    (cpath-navigation--mark-branch)
    (cpath-navigation--jump)))

(defun cpath-navigation-down nil
  (interactive)
  (let ((child (cpath-node-get-branch-child cpath-current-node)))
    (unless child
      (user-error "Cannot move down, at bottom"))
    (setq cpath-current-node child)
    (cpath-navigation--mark-branch)
    (cpath-navigation--jump)))

(defun cpath-navigation--branch (direction)
  "When DIRECTION is :next, set the branch child to the one following the
current one. When DIRECTION is :prev, set the branch child to the one preceding
the current one."
  (let ((children (plist-get cpath-current-node :children))
        branch-child new-branch-child)
    (unless (or (null children) (null (cdr children)))
      (setq branch-child (cpath-node-get-branch-child cpath-current-node)
            new-branch-child (my-list-neighbor children branch-child direction))
      (cpath-node-set-branch-child cpath-current-node new-branch-child)
      (cpath-navigation--mark-branch))))

(defun cpath-navigation-branch-next nil
  "Set the branch child to the one preceding the current one"
  (interactive)
  (cpath-navigation--branch :next))

(defun cpath-navigation-branch-prev nil
  "Set the branch child to the one preceding the current one"
  (interactive)
  (cpath-navigation--branch :prev))

(defun cpath-navigation-next-tree nil
  "Set the current node to be the root of the top-level tree following the current one"
  (interactive)
  (cpath--check-current)
  (let* ((current-tree (cpath--current-root))
         (next-tree (my-list-neighbor cpath-trees current-tree :next)))
    (unless (eq current-tree next-tree)
      (setq cpath-current-node next-tree))
    (cpath-navigation--mark-branch)))

(defun cpath-navigation-prev-tree nil
  "Set the current node to be the root of the top-level tree preceding the current one"
  (interactive)
  (cpath--check-current)
  (let* ((current-tree (cpath--current-root))
         (prev-tree (my-list-neighbor cpath-trees current-tree :prev)))
    (unless (eq current-tree prev-tree)
      (setq cpath-current-node prev-tree))
    (cpath-navigation--mark-branch)))

(defun cpath-navigation-quit nil
  (interactive)
  (kill-buffer-and-window))

;;########################################
;; keymap
(defvar cpath-map (make-sparse-keymap))
(progn
  (define-key cpath-map "c" 'cpath-goto-current)
  (define-key cpath-map "p" 'cpath-up)
  (define-key cpath-map "n" 'cpath-down)
  (define-key cpath-map "d" 'cpath-prune)
  (define-key cpath-map " " 'cpath-call)
  (define-key cpath-map "v" 'cpath-navigate))
(define-key prog-mode-map "\C-cp" cpath-map)
;;########################################
(provide 'cpath)
