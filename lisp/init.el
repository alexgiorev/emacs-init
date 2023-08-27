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

;;════════════════════════════════════════
;; makes it easier to define keys

(defsubst my-mode-hook-name (mode)
  "Construct a mode-hook name based on a MODE name."
  (intern (concat (symbol-name mode) "-hook")))

(defsubst my-mode-map-name (mode)
  "Construct a map name based on a MODE name."
  (intern (concat (symbol-name mode) "-map")))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(require 'my-org)
(require 'my-code)
(require 'my-macs)
(require 'my-python)
(require 'my-elisp)
(require 'my-js)
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

;; ════════════════════════════════════════
;; misc
(show-paren-mode)

(require 'undo-tree)
(global-undo-tree-mode)

(global-set-key "\C-ha" 'apropos)

(global-set-key "\C-x\C-n" nil)

(when (get-buffer "*scratch*")
  (kill-buffer "*scratch*")
  (find-file-noselect "~/scratch/scratch.el")
  (find-file-noselect "~/scratch/scratch.py")
  (find-file-noselect "~/scratch/scratch.js")
  (find-file-noselect "~/scratch/scratch.org"))

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

(defun my-all-defuns (&optional buffer)
  "Returns all symbols in the current buffer which are defined in a defun."
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

(defun my-insert-ruler (length)
  (interactive "p")
  (insert (make-string length ?═)))
(define-key global-map (kbd "C-x C-r") 'my-insert-ruler)

(defun my-directory-non-directory-files (dir &optional full pred)
  (let ((all-files (directory-files dir :full))
        result)
    (setq result (seq-filter (lambda (file) (not (file-directory-p file)))
                             all-files))
    (unless full
      (setq result (mapcar 'file-name-nondirectory result)))
    (when pred
      (setq result (seq-filter pred result)))
    result))

(defvar my-leng-dir "~/notes/leng")
(defun my-leng-files (&optional full)
  "Returns a list of the leng files. Currently these are the files in ~/leng
  which are not excluded (via leng-exclude.el)"
  (let ((leng-exclude (read (with-temp-buffer
                              (insert-file-contents "~/leng/leng-exclude.el")
                              (buffer-string)))))
    (my-directory-non-directory-files
     my-leng-dir full
     (lambda (file)
       (not (member (file-name-nondirectory file) leng-exclude))))))

;; So that C-i isn't TAB, and C-m isn't RET
;; (define-key input-decode-map [?\C-m] [C-m])
;; (define-key input-decode-map [?\C-i] [C-i])

(setq reverse-im-input-methods '("bulgarian-phonetic"))

;; Just copied the lambda macro.
(defmacro λ (&rest cdr)
  "Return an anonymous function.
Under dynamic binding, a call of the form (lambda ARGS DOCSTRING
INTERACTIVE BODY) is self-quoting; the result of evaluating the
lambda expression is the expression itself.  Under lexical
binding, the result is a closure.  Regardless, the result is a
function, i.e., it may be stored as the function value of a
symbol, passed to `funcall' or `mapcar', etc.

ARGS should take the same form as an argument list for a `defun'.
DOCSTRING is an optional documentation string.
 If present, it should describe how to call the function.
 But documentation strings are usually not useful in nameless functions.
INTERACTIVE should be a call to the function `interactive', which see.
It may also be omitted.
BODY should be a list of Lisp expressions.

\(fn ARGS [DOCSTRING] [INTERACTIVE] BODY)"
  (declare (doc-string 2) (indent defun)
           (debug (&define lambda-list lambda-doc
                           [&optional ("interactive" interactive)]
                           def-body)))
  ;; Note that this definition should not use backquotes; subr.el should not
  ;; depend on backquote.el.
  (list 'function (cons 'lambda cdr)))

;;════════════════════════════════════════

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

;;════════════════════════════════════════

(defun my-white-noise ()
  (interactive)
  (start-process "white-noise" nil "xdg-open"
                 "/home/alex/Downloads/white-noise.mp3"))

;;════════════════════════════════════════

(defun my-pomo ()
  (interactive)
  (call-process "gnome-pomodoro" nil 0))

;;════════════════════════════════════════

(defun my-firefox ()
  (interactive)
  (start-process "firefox" nil "/usr/bin/firefox"))
(put 'dired-find-alternate-file 'disabled nil)

;;════════════════════════════════════════

(add-hook
 'dired-mode-hook
 (lambda ()
   (define-key dired-mode-map (kbd "\C-c\C-p")
     (lambda () (interactive) (find-alternate-file "..")))))

;;════════════════════════════════════════
;; avoid “cannot set terminal process group” error
(setq shell-command-switch "-c")

;;════════════════════════════════════════
;; directories before files in dired
(setq dired-listing-switches "-agho --group-directories-first")

;;════════════════════════════════════════
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

;;════════════════════════════════════════
(defun my-Info-save-heading ()
  "Saves the title of the current node into the kill ring."
  (interactive)
  (kill-new Info-current-node))
(with-eval-after-load 'info
  (define-key Info-mode-map (kbd "C-c C-t") 'my-Info-save-heading))

;;════════════════════════════════════════

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

;;════════════════════════════════════════
(defun my-randchoice (list)
  (nth (random (length list)) list))

;;════════════════════════════════════════
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

;;════════════════════════════════════════
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

;;════════════════════════════════════════
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

;;════════════════════════════════════════
(defun my-inc-region (arg start end)
  (interactive "p\nr")
  (save-excursion
    (save-restriction
      (goto-char start)
      (narrow-to-region start end)
      (while (re-search-forward "\\([[:digit:]]+\\)" nil t)
        (let ((int (string-to-number (match-string 0))))
          (replace-match (number-to-string (+ int arg))))))))

;;════════════════════════════════════════
;; Emacs sentences begin with two blanks, but mine begin with only one
(defun my-mark-sentence ()
  (interactive)
  (unless (region-active-p)
    (push-mark)
    (activate-mark))
  (re-search-forward "[.!?,;]"))

(define-key global-map (kbd "M-e") 'my-mark-sentence)
;;════════════════════════════════════════
;; unfill-region binding
(global-set-key (kbd "C-c u") 'unfill-region)

;; indirect buffer key-binding
(global-set-key (kbd "C-c i") 'clone-indirect-buffer)

;; view mode instead of read only mode
(global-set-key (kbd "C-x C-q") 'view-mode)

;;════════════════════════════════════════
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

;;════════════════════════════════════════
(defvar my-global-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-t" 'my-touch-left)
    map)
"The map containing the bindings of my own commands in the global map")

(define-key global-map "\C-\M-m" my-global-prefix-map)

;;════════════════════════════════════════

(defun my-collect-symbol-definitions ()
  "Returns a list of the symbols which are in a top-level definition in the
current buffer"
  (seq-filter 'identity
              (mapcar (lambda (form)
                        (when (memq (car-safe form) '(defvar defun))
                          (cadr form)))
                      (my-read-buffer))))

(defun my-apropos-current-buffer nil
  (interactive)
  (require 'apropos)
  (apropos-symbols-internal (my-collect-symbol-definitions) nil))

;;════════════════════════════════════════
;; * registers
(set-register ?r "(region-beginning) (region-end)")
(set-register ?p "(point-min) (point-max)")
(set-register ?u "*{UPDATE}* ")
(set-register ?t "*{THROWN}* ")
(set-register ?m "*{me}* ")
(set-register ?b "*{book}* ")
(set-register ?a "*⟶*")
(set-register ?c "*{comment}* ")
(set-register ?. "[...]")

(defvar my-insert-math-char-alist
  '((?i . "∫") (?e . "ε") (?d . "δ")
    (?a . "α") (?b . "β") (?g . "γ")
    (?v . "∀") (?x . "∃") (?l . "λ")))
(defun my-insert-math-char nil
  (interactive)
  (let* ((code (read-char-exclusive))
         (char (cdr (assoc code my-insert-math-char-alist))))
    (when char (insert char))))
(global-set-key (kbd "C-x m") 'my-insert-math-char)

;;════════════════════════════════════════
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
  (define-key my-magit-map "s" 'magit-status)
  (define-key my-magit-map "d" 'magit-dispatch))
(global-set-key "\C-xg" my-magit-map)

(defsubst my-magit-section-type nil
  "Return the type of the section at point"
  (caar (magit-section-ident (magit-current-section))))

(defsubst my-magit-section-value nil
  "Return the value of the section at point"
  (cdar (magit-section-ident (magit-current-section))))

(defsubst my-magit-current-commit nil
  (let ((type+value (car (magit-section-ident (magit-current-section)))))
    (when (eq 'commit (car type+value))
      (cdr type+value))))

(defun my-magit-log-commits nil
  (org-with-point-at 1
    (while (not (eq 'commit (my-magit-section-type)))
      (forward-line))
    (let* ((result-head (cons nil nil))
           (result-tail result-head))
      (while (eq 'commit (my-magit-section-type))
        (setq result-tail
              (setcdr result-tail
                      (cons (my-magit-section-value) nil)))
        (forward-line))
      (cdr result-head))))

(defun my-magit-log-function-list (file func)
  "Return a list of the texts of FUNC in each version of FILE that appears in
one of the commits listed in the current buffer, assumed to be a magit log
buffer"
  (mapcar (lambda (commit)
            (my-magit-get-elisp-function commit file func))
          (my-magit-log-commits)))

(defvar my-magit-log-save-var (cons nil nil)
  "A pair (FILE . FUNC). This defines the function `my-magit-log-save' will save
  in the kill ring based on the current commit")

(defun my-magit-log-save-func nil
  "Save in the kill ring the text of FUNC as appearing in FILE as appearing in
the current commit, where FILE and FUNC are defined in `my-magit-log-save-var'"
  (interactive)
  (kill-new (my-magit-get-elisp-function
             (my-magit-current-commit)
             (car my-magit-log-save-var)
             (cdr my-magit-log-save-var))))

(defun my-magit-get-elisp-function (commit file funcname)
  "Return the text of the function named FUNCNAME as it is in FILE as it was at COMMIT"
  (magit-with-blob commit file
    (emacs-lisp-mode)
    (my-elisp-get-function-text funcname)))


;;════════════════════════════════════════
;; buffer-forest

(defvar buffer-forest (forest))

(defun buffer-forest--check nil
  (when (forest-empty-p buffer-forest)
    (user-error "Forest is empty"))
  (unless (forest-current buffer-forest)
    (user-error "No node selected")))

(defun buffer-forest-switch-to-current nil
  (interactive)
  (buffer-forest--check)
  (buffer-forest--switch))

(defsubst buffer-forest--switch nil
  (switch-to-buffer (plist-get (forest-current buffer-forest) :buffer)))

(defun buffer-forest-new-node (&optional arg)
  "Create a node associated with the current buffer as a child of the current
node and make it the current node. When no node is selected, or when called with
a prefix argument, make the current buffer a root node."
  (interactive "P")
  (cond ((equal arg '(4))
         (forest-new-root buffer-forest :buffer (current-buffer)))
        ((equal arg '(16))
         (forest-new-parent buffer-forest :buffer (current-buffer)))
        (t
         (forest-new-child buffer-forest :buffer (current-buffer))))
  (message "New node: %s" (buffer-name (current-buffer))))

(defun buffer-forest-up nil
  (interactive)
  (buffer-forest--check)
  (forest-goto-parent buffer-forest)
  (buffer-forest--switch))

(defun buffer-forest-down nil
  (interactive)
  (buffer-forest--check)
  (forest-goto-child buffer-forest)
  (buffer-forest--switch))

(defun buffer-forest-next nil
  "Go to the next sibling. When on the last child of the parent, cycle back to the first one"
  (interactive)
  (buffer-forest--check)
  (forest-goto-sibling buffer-forest :next)
  (buffer-forest--switch))

(defun buffer-forest-prev nil
  "Go to the previous sibling. When on the first child of the parent, cycle to the last one"
  (interactive)
  (buffer-forest--check)
  (forest-goto-sibling buffer-forest :prev)
  (buffer-forest--switch))

(defun buffer-forest-new-sibling-next nil
  "Insert the current buffer as a node after the current node and make the new
node the current one"
  (interactive)
  (buffer-forest--check)
  (forest-new-sibling buffer-forest :next :buffer (current-buffer))
  (message "New node"))

(defun buffer-forest-new-sibling-prev nil
  "Insert the current buffer as a node before the current node and make the new
node the current one"
  (interactive)
  (buffer-forest--check)
  (forest-new-sibling buffer-forest :prev :buffer (current-buffer))
  (message "New node"))
      
(defun buffer-forest-prune nil
  (interactive)
  (buffer-forest--check)
  (forest-prune buffer-forest)
  (when (forest-empty-p buffer-forest)
    (message "Forest is now empty"))
  (buffer-forest--switch))

(defun buffer-forest-select nil
  (interactive)
  (buffer-forest--check)
  (let ((forest-name-func
         (lambda (node) (buffer-name (plist-get node :buffer)))))
    (when (forest-select buffer-forest)
      (buffer-forest--switch))))

(defun buffer-forest-remove-buffer-nodes (buffer)
  "Remove all nodes (and the trees they define) in the `buffer-trees' forest which hold BUFFER"
  (forest-prune-all buffer-forest
    (lambda (node) (eq (plist-get node :buffer) buffer))))

(defsubst buffer-forest-before-kill-buffer nil
  (buffer-forest-remove-buffer-nodes (current-buffer)))
(add-hook 'kill-buffer-hook 'buffer-forest-before-kill-buffer)

(defun buffer-forest-change-buffer nil
  "Sets the current buffer as the buffer of the current forest node"
  (interactive)
  (buffer-forest--check)
  (plist-put (forest-current buffer-forest) :buffer (current-buffer)))

;;════════════════════════════════════════
;; buffer-forest save and restore

(defvar buffer-forest-persist-file "~/leng/last-buffer-forest.el")
(defun buffer-forest-persist-forest nil
  (interactive)
  (unless (forest-empty-p buffer-forest)
    (with-temp-file buffer-forest-persist-file
      (let ((sexps (mapcar 'buffer-forest--to-sexp
                           (plist-get buffer-forest :children))))       
        (pp sexps (current-buffer))))))
(add-hook 'kill-emacs-hook 'buffer-forest-persist-forest)

(defun buffer-forest-load-last-forest nil
  (interactive)
  (setq buffer-forest (forest))
  (let (trees)
    (with-temp-buffer
      (insert-file-contents buffer-forest-persist-file)
      (beginning-of-buffer)
      (setq trees (read (current-buffer))))
    (dolist (tree trees)
      (buffer-forest--from-sexp tree))))

(defun buffer-forest-save-tree nil
  "Put in the kill ring a serialization of the current buffer-forest tree
This is the tree whose root is the foremost ancestor of the current node."
  (interactive)
  (let ((sexp (buffer-forest--to-sexp (forest-current-root buffer-forest))))
    (with-temp-buffer
      (pp sexp (current-buffer))
      (kill-new (buffer-substring-no-properties (point-min) (point-max))))))

(defun buffer-forest--to-sexp (node)
  (let* ((buffer (plist-get node :buffer))
         (file (or (buffer-file-name buffer)
                   (buffer-file-name (buffer-base-buffer buffer))))
         (name (buffer-name buffer))
         (org-node-id
          (with-current-buffer buffer
            (when (and (derived-mode-p 'org-mode) (buffer-narrowed-p))
              (save-excursion
                (goto-char (point-min))
                (unless (or (org-before-first-heading-p)
                            (save-excursion (org-goto-sibling)))
                  (my-org-custom-id-get-create))))))
         (indirect-p (not (null (buffer-base-buffer buffer)))))
    (when file
      `(:file ,file
        :org-node-id ,org-node-id
        :name ,name
        :indirect-p ,indirect-p
        :children ,(-keep 'buffer-forest--to-sexp (plist-get node :children))))))

(defun buffer-forest-restore nil
  "Assumes that point is after the sexp"
  (interactive)
  (buffer-forest--from-sexp (elisp--preceding-sexp)))

(defun buffer-forest--from-sexp (sexp)
  (let ((initial (forest-current buffer-forest))
        (root (forest-new-root buffer-forest)))
    (buffer-forest--from-sexp-walk sexp)
    (forest-set-current buffer-forest root)))

(defun buffer-forest--from-sexp-walk (node)
  "NODE is a SEXP node. Assumes that the `buffer-forest's current node is the
one which corresponds to NODE"
  (let* ((current (forest-current buffer-forest))
         (base-buffer (find-file-noselect (plist-get node :file)))
         (buffer (or (get-buffer (plist-get node :name))
                     (if (plist-get node :indirect-p)
                         (with-current-buffer base-buffer
                           (clone-indirect-buffer (plist-get node :name) nil))
                       base-buffer)))
         (org-node-id (plist-get node :org-node-id)))
    (when org-node-id
      (with-current-buffer buffer
        (widen)
        (when (ignore-errors (org-link-search (format "#%s" org-node-id)))
          (org-narrow-to-subtree))))
    (plist-put current :buffer buffer)
    (dolist (child (plist-get node :children))
      (forest-new-child buffer-forest)
      (buffer-forest--from-sexp-walk child)
      (forest-set-current buffer-forest current))))

;;════════════════════════════════════════
;; buffer map

(defvar my-buffer-map (make-sparse-keymap))
(progn
  (define-key my-buffer-map "\C-f" 'buffer-forest-next)
  (define-key my-buffer-map "\C-b" 'buffer-forest-prev)
  (define-key my-buffer-map "\C-p" 'buffer-forest-up)
  (define-key my-buffer-map "\C-n" 'buffer-forest-down)
  (define-key my-buffer-map "\C-c" 'buffer-forest-switch-to-current)
  (define-key my-buffer-map "\C-k" 'buffer-forest-prune)
  (define-key my-buffer-map "\C-e" 'buffer-forest-select)
  (define-key my-buffer-map "f" 'buffer-forest-new-sibling-next)
  (define-key my-buffer-map "b" 'buffer-forest-new-sibling-prev)
  (define-key my-buffer-map (kbd "C-SPC") 'buffer-forest-new-node)
  (define-key my-buffer-map "\C-l" 'list-buffers))
(progn 
  (global-set-key [up] 'buffer-forest-up)
  (global-set-key [down] 'buffer-forest-down)
  (global-set-key [right] 'buffer-forest-next)
  (global-set-key [left] 'buffer-forest-prev)
  (global-set-key "\C-x\C-b" my-buffer-map))

;;════════════════════════════════════════
;; calc
(setq calc-display-trail nil)

