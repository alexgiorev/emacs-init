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
  (car (seq-drop-while (lambda (node)
                         (not (plist-get node :branch)))
                       (plist-get node :children))))

(defsubst cpath-node-leafp (node)
  (null (plist-get node :children)))

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

;;----------------------------------------
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
  (let ((child (cpath-node-get-branch-child cpath-current-node)))
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
    (define-key map (kbd "RET") 'cpath-navigation-quit))
  (setq cpath-navigation-mode-map map))

(defvar cpath-navigation-branch-face nil
  "The face of the headings corresponding to the nodes of the current branch")
(defvar cpath-navigation-current-face nil
  "The face of the heading corresponding to the current node")

(define-derived-mode cpath-navigation-mode special-mode "cpath-navigation"
  ;; ----------------------------------------
  ;; insert an org-like representation of `cpath-trees', additionally
  ;; associating via text properties each heading with an actual node
  (read-only-mode -1)
  (erase-buffer)
  (dolist (tree cpath-trees)
    (cpath-depth-first-walk
     (lambda (node depth)
       (insert (make-string (* depth 2) ?_)
               (plist-get node :name) "\n")
       (beginning-of-line 0)
       (put-text-property (point) (1+ (point)) :cpath-node node)
       (beginning-of-line 2))
     tree)
    (insert "\n"))
  (read-only-mode 1)
  ;; ----------------------------------------
  (cpath-navigation--mark-branch)
  (cpath-navigation--goto-node cpath-current-node)
  (setq cursor-type nil))

(defun cpath-navigation--goto-node (node)
  "Moves point to the heading which corresponds to NODE"
  (beginning-of-buffer)
  (text-property-search-forward :cpath-node node 'eq)
  (beginning-of-line))

(defun cpath-navigation--heading-node nil
  "Returns the node corresponding to the heading at point"
  (beginning-of-line)
  (get-text-property (point) :cpath-node))

(defun cpath-navigation--mark-branch nil
  "Mark the current branch and leave point on the heading corresponding to the
current node"
  ;; unmark everything
  (remove-overlays (point-min) (point-max) :cpath-navigation t)
  ;; mark the current branch
  (let ((current (cpath--current-root)))
    (while current
      (cpath-navigation--goto-node current)
      (cpath-navigation--mark-heading
       (if (eq current cpath-current-node) :current :branch))
      (setq current (cpath-node-get-branch-child current)))))

(defun cpath-navigation--mark-heading (type)
  "Assumes point is at the beginning of a heading. When TYPE is :branch, marks
it visually as a part of the current branch. When TYPE is :current, marks the
heading as the one which maps to the current node."
  (let ((overlay (make-overlay (point) (line-end-position)))
        (face (cond ((eq type :current) '(:foreground "white" :background "black"))
                    ((eq type :branch) '(:foreground "black" :background "yellow"))
                    (t (error "Invalid type: %s" type)))))
    ;; mark the overlay with this property so that we can delete it later
    (overlay-put overlay :cpath-navigation t)
    (overlay-put overlay 'face face)))

(defun cpath-navigation--unmark-heading nil
  "Assumes point is at the beginning of a heading. Ensures the heading is not
  marked as a node in the branch."
  (remove-overlays (point) (line-end-position) :cpath-navigation t))

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
            new-branch-child (my-list-neighbor children branch-child (eq direction :prev)))
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
  "Set the current node to be the root of the top-level tree following the
current one"
  (interactive)
  (cpath--check-current)
  (error "TODO"))

(defun cpath-navigation-quit nil
  (interactive)
  (kill-buffer-and-window))

;; ----------------------------------------
;; (experimental) alternative tree visualization

(defvar my-tree-node-property (make-symbol ":node")
  "Used as the text property which associates the visualization of the node with
the node object.")

(defun my-tree-draw (&optional root)
  "Make sure to call on an empty line"
  (setq root (or root (cpath--current-root)))
  (insert (my-tree-draw-node root)))

(defun my-tree-draw-node (node)
  "Draw NODE at point and leave point at the end of the drawing"
  (let ((children-markers nil)
        (children (plist-get node :children))
        child-start child-end connector node-string)
    (if (not children)
        (insert (plist-get node :name) "\n")
      (save-restriction
        (narrow-to-region (point) (point))
        (dolist (child children)
          (push (point-marker) children-markers)
          (my-tree-draw-node child))
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
        (save-excursion (insert node-string "\n"))
        (put-text-property (point) (1+ (point)) my-tree-node-property node)
        (goto-char (point-max))))))

(defun my-tree-mark-branch (leaf)
  "Marks the branch defined by the node object LEAF.
Assumes that the current buffer displays a tree which features LEAF (i.e. that
there is text associated with LEAF through the `my-tree-node-property' text
property. Naturally, it also assumes that LEAF is actually a leaf node"
  (beginning-of-buffer)
  (text-property-search-forward my-tree-node-property leaf t)
  (end-of-line)
  (while (my-tree-mark--row)
    (my-tree-mark--column)))

(defun my-tree-mark--column nil
  "Assumes that point is on an already marked ╚ connector. Marks the column of
the connector (i.e. up to and including the first ╗ connector)"
  (let ((column (current-column)))
  (while (not (= (char-after) ?╗))
    (beginning-of-line 0) (forward-char column)
    (my-tree--mark (point) (1+ (point))))))

(defun my-tree-mark--row nil
  "Marks from point up to and including the first ╚ or ╠ connector. Returns nil when
there is no such connector (which means that the root has been marked), and t
otherwise."
  (let ((end (point)))
    (if (re-search-backward "╚\\|╠" nil t)
        (progn (my-tree--mark (point) end) t)
      (beginning-of-line)
      (my-tree--mark (point) end)
      nil)))

;; TODO: Use `defface'
(defvar my-tree-mark-face nil)
(setq my-tree-mark-face '(:foreground "black" :background "yellow"))
(defun my-tree--mark (start end)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face my-tree-mark-face)
    (overlay-put overlay :my-tree-mark t)
    (overlay-put overlay 'evaporate t)))
(defun my-tree--unmark nil
  (remove-overlays (point-min) (point-max) :my-tree-mark t))

;;----------------------------------------
;; keymap
(defvar cpath-map (make-sparse-keymap))
(progn
  (define-key cpath-map "c" 'cpath-call)
  (define-key cpath-map "p" 'cpath-up)
  (define-key cpath-map "n" 'cpath-down)
  (define-key cpath-map "d" 'cpath-prune)
  (define-key cpath-map " " 'cpath-goto-current)
  (define-key cpath-map "v" 'cpath-navigate))
(define-key prog-mode-map "\C-cp" cpath-map)
;;----------------------------------------
(provide 'cpath)
