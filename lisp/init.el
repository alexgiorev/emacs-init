;; Make it possible to download from MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

; hide the toolbar
(tool-bar-mode -1)

;;########################################
;; makes it easier to define keys

(defsubst my-mode-hook-name (mode)
  "Construct a mode-hook name based on a MODE name."
  (intern (concat (symbol-name mode) "-hook")))

(defsubst my-mode-map-name (mode)
  "Construct a map name based on a MODE name."
  (intern (concat (symbol-name mode) "-map")))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
;; org-mode configuration
(require 'my-org)
;; programming configuration
(require 'my-code)
;; python-configuration
(require 'my-python)
;; miscellaneous functions
(require 'my-macs)
;; emacs coding
(require 'my-elisp)
;; functions I don't want to share
(require 'private)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes nil)
 '(elpy-rpc-python-command "python3")
 '(package-selected-packages
   '(undo-tree bazel rust-mode projectile elpy magit anki-editor racket-mode))
 '(send-mail-function 'mailclient-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; If you want to use spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)

;; So that visual-line-mode is on always
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'fundamental-mode-hook 'visual-line-mode)

;; set fill-column globally
(setq-default fill-column 80)

;; org-mode is the default
(setq-default major-mode 'org-mode)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; C indentation 4 spaces, not 2
(setq-default c-basic-offset 4)

;; unfill region
(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(define-key global-map "\C-\M-Q" 'unfill-region)

;; so that shell reads .bashrc
(setq shell-command-switch "-ic")

;; show column numbers
(dolist (hook '(org-mode-hook python-mode-hook))
  (add-hook hook 'column-number-mode))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(define-key global-map (kbd "C-c f") 'fill-region)

;; enable horizontal scrolling
(put 'scroll-left 'disabled nil)

;; Disable menu, toolbar and scrollbar
(menu-bar-mode -1) 
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; misc
(show-paren-mode)

;; increase default text size
(require 'face-remap)
(defun my-text-size nil
  (unless (minibufferp)
    (setq text-scale-mode-amount 1)
    (text-scale-mode text-scale-mode-amount)))

(add-hook 'after-change-major-mode-hook
          'my-text-size)

(put 'set-goal-column 'disabled nil)

;; Dired

(setq dired-isearch-filenames t)

;;########################################
;; Miscellaneous commands
(defun my-all-defuns (&optional buffer)
  "Returns all in the current buffer symbols which are defined in a defun"
  (setq buffer (if buffer (get-buffer buffer) (current-buffer)))
  (save-excursion
    (goto-char (point-min))
    (let (symbols)
      (condition-case nil
          (while t
            (let ((expr (read buffer)))
              (if (eq (car-safe expr) 'defun)
                  (setq symbols (cons (cadr expr) symbols)))))
        (end-of-file))
      symbols)))

(defun my-print-defuns (&optional buffer)
  (setq buffer (if buffer (get-buffer buffer) (current-buffer)))
  (with-output-to-temp-buffer "defuns"
    (dolist (sym (my-all-defuns buffer))
      (princ (format "%s\n" sym)))))

;;########################################

(defun my-books ()
  (interactive)
  (find-file "~/Downloads/books"))

(add-hook
 'dired-mode-hook
 (lambda ()
   (define-key dired-mode-map "\C-cx" 'my-dired-xdg-open)))

(defun my-dired-xdg-open ()
  (interactive)
  (call-process "/usr/bin/xdg-open" nil 0 nil
                (dired-get-filename)))

;;########################################

(defun my-white-noise ()
  (interactive)
  (start-process "white-noise" nil "xdg-open"
                 "/home/alex/Downloads/white-noise.mp3"))

;;########################################

(defun my-pomo ()
  (interactive)
  (call-process "gnome-pomodoro" nil 0))

;;########################################

(defun my-firefox ()
  (interactive)
  (start-process "firefox" nil "/usr/bin/firefox"))
(put 'dired-find-alternate-file 'disabled nil)

;;########################################

(add-hook
 'dired-mode-hook
 (lambda ()
   (define-key dired-mode-map (kbd "\C-c\C-p")
     (lambda () (interactive) (find-alternate-file "..")))))

;;########################################
;; avoid “cannot set terminal process group” error
(setq shell-command-switch "-c")

;;########################################
;; directories before files in dired
(setq dired-listing-switches "-agho --group-directories-first")

;;########################################
(defun my-narrow-indentation ()
  (interactive)
  (beginning-of-line)
  (let ((beginning (point))
        (level (current-indentation)))
    (forward-line)
    (while (or (and (not (eobp))
                    (> (current-indentation) level))
               (looking-at "[[:blank:]]*$"))
      (forward-line))
    (narrow-to-region beginning (point))
    (goto-char beginning)))

(global-set-key (kbd "C-x n i") 'my-narrow-indentation)

;;########################################
(defun my-Info-save-heading ()
  "Saves the title of the current node into the kill ring."
  (interactive)
  (kill-new Info-current-node))

(add-hook 'Info-mode-hook
          (lambda ()
            (define-key Info-mode-map
              (kbd "C-c t")
              'my-Info-save-heading)))

;;########################################

(defun my-forward-line-no-indentation (&optional arg)
  "Move to the next line having no indentation. Skip empty lines"
  (interactive "p")
  (let ((direction (if (< arg 0) -1 1))
        (arg (abs arg)))
    (while (> arg 0)
      (while (if (= (forward-line direction) 0)
                 (if (and (= (current-indentation) 0)
                          (not (= (char-after) ?\n)))
                     (progn (setq arg (1- arg)) nil)
                   t)
               (setq arg 0) nil)))))

(with-eval-after-load "apropos"
  (define-key apropos-mode-map "n" 'my-forward-line-no-indentation)
  (define-key apropos-mode-map "p"
    (lambda nil (interactive) (my-forward-line-no-indentation -1))))

;;########################################
(defun my-randchoice (list)
  (nth (random (length list)) list))

;;########################################
;; finding files

(defun my-find-file (dir)
  (find-file (read-file-name "Find file: " dir)))

(defvar my-find-map (make-sparse-keymap))
(progn
  (define-key my-find-map "l"
    (lambda nil (interactive) (my-find-file "~/leng/")))
  (define-key my-find-map "e"
    (lambda nil (interactive) (my-find-file (concat user-emacs-directory "lisp/"))))
  (define-key my-find-map "i" 'find-library))
(global-set-key "\C-xf" my-find-map)

;;########################################
(defun my-dired-store-filename-in-kill-ring ()
  (interactive)
  (kill-new (dired-get-filename)))

(with-eval-after-load 'dired
  (define-key dired-mode-map "\C-ck"
    'my-dired-store-filename-in-kill-ring))

(defun my-org-dired-store-link-xdg-open ()
  "Stores a link which when invoked will run xdg-open on the file at point."
  (interactive)
  (let* ((file (dired-get-filename))
         (form `(call-process "xdg-open" nil 0 nil ,file)))
    (push (list (format "elisp:%S" form) (file-name-nondirectory file))
          org-stored-links)))

;;########################################
;; fix a bug, change :desc to :description
(defun org-info-store-link ()
  "Store a link to an Info file and node."
  (when (eq major-mode 'Info-mode)
    (let ((link (concat "info:"
			(file-name-nondirectory Info-current-file)
			"#" Info-current-node))
	  (desc (concat (file-name-nondirectory Info-current-file)
			"#" Info-current-node)))
      (org-link-store-props :type "info" :file Info-current-file
			    :node Info-current-node
			    :link link :description desc)
      link)))

;;########################################
(defun my-inc-region (arg start end)
  (interactive "p\nr")
  (save-excursion
    (save-restriction
      (goto-char start)
      (narrow-to-region start end)
      (while (re-search-forward "\\([[:digit:]]+\\)" nil t)
        (let ((int (string-to-number (match-string 0))))
          (replace-match (number-to-string (+ int arg))))))))

;;########################################
;; Emacs sentences begin with two blanks, but mine begin with only one
(defun my-mark-sentence ()
  (interactive)
  (unless (region-active-p)
    (push-mark)
    (activate-mark))
  (re-search-forward "[.!?,;]"))

(define-key global-map (kbd "M-e") 'my-mark-sentence)
;;########################################
;; unfill-region binding
(global-set-key (kbd "C-c u") 'unfill-region)

;; indirect buffer key-binding
(global-set-key (kbd "C-c i") 'clone-indirect-buffer)

;; view mode instead of read only mode
(global-set-key (kbd "C-x C-q") 'view-mode)

;;########################################
(defun my-dired-kill-dot-files ()
  (interactive)
  (dired-mark-files-regexp "^\\.")
  (dired-do-kill-lines))

(with-eval-after-load 'dired
  (define-key dired-mode-map "\C-cd" 'my-dired-kill-dot-files))

(defun my-dired-goto-marked-file (previous)
  (let (func pos)
    (save-excursion
      (if previous
          (progn (beginning-of-line 1) (setq func 're-search-backward))
        (beginning-of-line 2) (setq func 're-search-forward))
      (setq pos (funcall func "^\\*" nil t)))
    (if pos
        (progn (goto-char pos) (dired-goto-next-file))
      (message "No marked files in that direction"))))

(with-eval-after-load 'dired
  (define-key dired-mode-map
    (kbd "C-c C-n")
    (lambda nil
      (interactive)
      (my-dired-goto-marked-file nil)))
  (define-key dired-mode-map
    (kbd "C-c C-p")
    (lambda nil
      (interactive)
      (my-dired-goto-marked-file t))))

;;########################################
(defvar my-global-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-t" 'my-touch-left)
    map)
"The map containing the bindings of my own commands in the global map")

(define-key global-map "\C-\M-m" my-global-prefix-map)

;;########################################

(defun my-collect-symbol-definitions ()
  "Returns a list of the symbols which are in a top-level definition in the
current buffer"
  (seq-filter 'identity
              (mapcar (lambda (form)
                        (when (memq (car-safe form) '(defvar defun))
                          (cadr form)))
                      (my-read-buffer))))

(defun my-apropos-current-buffer ()
  (interactive)
  (require 'apropos)
  (apropos-symbols-internal (my-collect-symbol-definitions) nil))

;;########################################
(global-set-key "\C-ha" 'apropos)

;;########################################
(global-set-key "\C-x\C-n" nil)

;;########################################
;; * registers
(set-register ?r "(region-beginning) (region-end)")

;;########################################
(when (get-buffer "*scratch*")
  (kill-buffer "*scratch*")
  (find-file-noselect "/tmp/scratch.el"))

;;########################################
(require 'undo-tree)
(global-undo-tree-mode)

;;########################################
;; magit

(defun my-magit-commit-file (&optional ammend)
  "Stage the current file and commit. For convenience, attempts to put in the
kill ring the name of the defun at point."
  (interactive)
  (ignore-errors
    (if (eq major-mode 'python-mode)
        (my-python-save-name)
      (my-elisp-save-defun-name)))
  (magit-stage-file (buffer-file-name))
  (if ammend (magit-commit-amend) (magit-commit)))

(defvar my-magit-map (make-sparse-keymap))
(progn
  (define-key my-magit-map "c" 'my-magit-commit-file)
  (define-key my-magit-map "a"
    (lambda nil (interactive) (my-magit-commit-file :amend)))
  (define-key my-magit-map "p" 'magit-push-current-to-upstream)
  (define-key my-magit-map "s" 'magit-status))
(global-set-key "\C-xg" my-magit-map)

;;########################################
;; * buffer navigation

(defun my-buffer-marry (buffer-or-name)
  "Set BUFFER-OR-NAME as the spouse of the current buffer
This facilitates switching between two buffers which are related"
  (interactive "bMarry with: ")
  (let ((current-buffer (current-buffer)))
    (setq-local my-buffernav-spouse buffer-or-name)
    (with-current-buffer buffer-or-name
      (setq-local my-buffernav-spouse current-buffer))))

(defun my-buffer-switch-to-spouse nil
  (interactive)
  (unless (boundp 'my-buffernav-spouse)
    (user-error "Current buffer has no spouse"))
  (switch-to-buffer my-buffernav-spouse))

;;####################
;; ** buffer-tree

(defvar buffer-trees nil
  "The roots of the forest")
(defvar buffer-tree-current nil
  "The current node in the forest")

(defun buffer-tree--check-current nil
  (unless buffer-tree-current
    (user-error "No node selected")))

(defun buffer-tree-switch-to-current nil
  (interactive)
  (buffer-tree--check-current)
  (buffer-tree--switch))

(defsubst buffer-tree--switch nil
  (switch-to-buffer (plist-get buffer-tree-current :buffer)))

(defun buffer-tree-new-node (&optional root-p)
  "Append the current buffer as a child of the current node and make it the
current node. When no node is selected, or when called with a prefix argument,
make the current buffer a root node."
  (interactive "P")
  (let* ((buffer (current-buffer))
         parent node)
    (if (or root-p (null buffer-tree-current))
        (progn (setq node (list :buffer buffer))
               (setq buffer-trees (append buffer-trees (list node))))
      (when (eq (plist-get buffer-tree-current :buffer) buffer)
        (user-error "The current node already references the current buffer"))
      (setq node (list :buffer buffer :parent buffer-tree-current))
      (plist-put buffer-tree-current :children
                 (append (plist-get buffer-tree-current :children)
                         (list node))))
    (setq buffer-tree-current node)
    (message "New node: %s" (buffer-name buffer))))

(defun buffer-tree-up nil
  (interactive)
  (buffer-tree--check-current)
  (let ((parent (plist-get buffer-tree-current :parent)))
    (unless parent
      (user-error "Cannot go up, on a root"))
    (setq buffer-tree-current parent))
  (buffer-tree--switch))

(defun buffer-tree-down nil
  "Move to the first child"
  (interactive)
  (buffer-tree--check-current)
  (let ((children (plist-get buffer-tree-current :children)))
    (unless children
      (user-error "Cannot go down, on a leaf"))
    (setq buffer-tree-current (car children)))
  (buffer-tree--switch))

(defun buffer-tree-next nil
  "Go to the next sibling. When on the last child of the parent, cycle back to the first one"
  (interactive)
  (let* ((parent (plist-get buffer-tree-current :parent))
         (sibling (my-list-neighbor
                   (if parent (plist-get parent :children) buffer-trees)
                   buffer-tree-current)))
    (if (eq sibling buffer-tree-current)
        (message "next same as current")
      (setq buffer-tree-current sibling)
      (buffer-tree--switch))))

(defun buffer-tree-prev nil
  "Go to the previous sibling. When on the first child of the parent, cycle to the last one"
  (interactive)
  (let* ((parent (plist-get buffer-tree-current :parent))
         (sibling (my-list-neighbor
                   (if parent (plist-get parent :children) buffer-trees)
                   buffer-tree-current :left)))
    (if (eq sibling buffer-tree-current)
        (message "prev same as current")
      (setq buffer-tree-current sibling)
      (buffer-tree--switch))))

(defun buffer-tree-new-sibling-after nil
  "Insert a the current buffer as a node after the current node and make the new
node the current one"
  (interactive)
  (buffer-tree--check-current)
  (let ((parent (plist-get buffer-tree-current :parent))
        node)
    (if parent
        (progn (setq node (list :buffer (current-buffer) :parent parent))
               (plist-put parent :children
                 (my-list-add-after (plist-get parent :children)
                                    buffer-tree-current
                                    node)))
      (setq node (list :buffer (current-buffer)))
      (my-list-add-after buffer-trees buffer-tree-current node))
    (setq buffer-tree-current node)))

(defun buffer-tree-new-sibling-before nil
  "Insert a the current buffer as a node after the current node and make the new
node the current one"
  (interactive)
  (buffer-tree--check-current)
  (let ((parent (plist-get buffer-tree-current :parent))
        node)
    (if parent
        (progn (setq node (list :buffer (current-buffer) :parent parent))
               (plist-put parent :children
                 (my-list-add-before (plist-get parent :children)
                                     buffer-tree-current
                                     node)))
      (setq node (list :buffer (current-buffer)))
      (my-list-add-before buffer-trees buffer-tree-current node))
    (setq buffer-tree-current node)))
      
(defun buffer-tree-pop nil
  "Remove the subtree of the current node and select the parent. This doesn't
kill the buffer. When the current node is a root, selects the next one."
  (interactive)
  (buffer-tree--check-current)
  (let* ((parent (plist-get buffer-tree-current :parent))
         next-current)
    (if parent
        (progn (plist-put parent :children
                          (delq buffer-tree-current (plist-get parent :children)))
               (setq next-current parent))
      (setq next-current (my-list-neighbor buffer-trees buffer-tree-current))
      (when (eq next-current buffer-tree-current)
        (setq next-current nil))
      (setq buffer-trees (delq buffer-tree-current buffer-trees)))
    (setq buffer-tree-current next-current)
    (if next-current
        (buffer-tree--switch)
      (message "Popped last node"))))

;; treevis functions
(defun buffer-tree-parent-func (node)
  (plist-get node :parent))
(defun buffer-tree-name-func (node)
  (buffer-name (plist-get node :buffer)))
(defun buffer-tree-children-func (node)
  (plist-get node :children))

(defun buffer-tree-select nil
  (interactive)
  (let* ((treevis-parent-func 'buffer-tree-parent-func)
         (treevis-name-func 'buffer-tree-name-func)
         (treevis-children-func 'buffer-tree-children-func)
         (node (treevis-select buffer-trees buffer-tree-current)))
    (when node
      (setq buffer-tree-current node)
      (buffer-tree--switch))))


;;####################
;; ** map

(defvar my-buffer-map (make-sparse-keymap))
(progn
  (define-key my-buffer-map "\C-f" 'buffer-tree-next)
  (define-key my-buffer-map "\C-b" 'buffer-tree-prev)
  (define-key my-buffer-map "\C-p" 'buffer-tree-up)
  (define-key my-buffer-map "\C-n" 'buffer-tree-down)
  (define-key my-buffer-map "\C-c" 'buffer-tree-switch-to-current)
  (define-key my-buffer-map "\C-k" 'buffer-tree-pop)
  (define-key my-buffer-map "f" 'buffer-tree-new-sibling-after)
  (define-key my-buffer-map "b" 'buffer-tree-new-sibling-before)
  (define-key my-buffer-map (kbd "C-SPC") 'buffer-tree-new-node)
  (define-key my-buffer-map "\C-l" 'list-buffers)
  (define-key my-buffer-map "\C-s" 'my-buffer-switch-to-spouse)
  (define-key my-buffer-map "\C-r" 'my-buffer-marry))
(progn 
  (global-set-key [up] 'buffer-tree-up)
  (global-set-key [down] 'buffer-tree-down)
  (global-set-key [right] 'buffer-tree-next)
  (global-set-key [left] 'buffer-tree-prev)
  (global-set-key "\C-x\C-b" my-buffer-map))
