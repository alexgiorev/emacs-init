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

;; ----------------------------------------
;; makes it easier to define keys

(defsubst my-mode-hook-name (mode)
  "Construct a mode-hook name based on a MODE name."
  (intern (concat (symbol-name mode) "-hook")))

(defsubst my-mode-map-name (mode)
  "Construct a map name based on a MODE name."
  (intern (concat (symbol-name mode) "-map")))

;; org-mode configuration
(load "~/.emacs.d/my-org")
;; programming configuration
(load "~/.emacs.d/code")
;; python-configuration
(load "~/.emacs.d/my-python")
;; miscellaneous functions
(require 'my-macs "~/.emacs.d/my-macs.el")
;; functions I don't want to share
(load "~/.emacs.d/private")

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
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (unless (string-match "*Minibuf-" (buffer-name))
              (text-scale-adjust 0) (text-scale-adjust 1) (message ""))))

(put 'set-goal-column 'disabled nil)

;; Dired

(setq dired-isearch-filenames t)

;; ----------------------------------------
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

;; ----------------------------------------

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

;; ----------------------------------------

(defun my-white-noise ()
  (interactive)
  (start-process "white-noise" nil "xdg-open"
                 "/home/alex/Downloads/white-noise.mp3"))

;; ----------------------------------------

(defun my-pomo ()
  (interactive)
  (call-process "gnome-pomodoro" nil 0))

;; ----------------------------------------

(defun my-firefox ()
  (interactive)
  (start-process "firefox" nil "/usr/bin/firefox"))
(put 'dired-find-alternate-file 'disabled nil)

;; ----------------------------------------

(add-hook
 'dired-mode-hook
 (lambda ()
   (define-key dired-mode-map (kbd "\C-c\C-p")
     (lambda () (interactive) (find-alternate-file "..")))))

;; ----------------------------------------
;; avoid “cannot set terminal process group” error
(setq shell-command-switch "-c")

;; ----------------------------------------
;; directories before files in dired
(setq dired-listing-switches "-agho --group-directories-first")

;; ----------------------------------------
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

;; ----------------------------------------
(defun my-Info-save-heading ()
  "Saves the title of the current node into the kill ring."
  (interactive)
  (kill-new Info-current-node))

(add-hook 'Info-mode-hook
          (lambda ()
            (define-key Info-mode-map
              (kbd "C-c t")
              'my-Info-save-heading)))

;; ----------------------------------------

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

;; ----------------------------------------
(defun my-randchoice (list)
  (nth (random (length list)) list))

;; ----------------------------------------
;; finding files

(defun my-find-file (dir)
  (find-file (read-file-name "Find file: " dir)))

(defvar my-find-map (make-sparse-keymap))
(progn
  (define-key my-find-map "l"
    (lambda nil (interactive) (my-find-file "~/leng/")))
  (define-key my-find-map "e"
    (lambda nil (interactive) (my-find-file user-emacs-directory)))
  (define-key my-find-map "i" 'find-library))
(global-set-key "\C-xf" my-find-map)

;; ----------------------------------------
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

;; ----------------------------------------
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

;; ----------------------------------------
(defun my-inc-region (arg start end)
  (interactive "p\nr")
  (save-excursion
    (save-restriction
      (goto-char start)
      (narrow-to-region start end)
      (while (re-search-forward "\\([[:digit:]]+\\)" nil t)
        (let ((int (string-to-number (match-string 0))))
          (replace-match (number-to-string (+ int arg))))))))

;; ----------------------------------------
;; Emacs sentences begin with two blanks, but mine begin with only one
(defun my-mark-sentence ()
  (interactive)
  (unless (region-active-p)
    (push-mark)
    (activate-mark))
  (re-search-forward "[.!?,;]"))

(define-key global-map (kbd "M-e") 'my-mark-sentence)
;; ----------------------------------------
;; unfill-region binding
(global-set-key (kbd "C-c u") 'unfill-region)

;; indirect buffer key-binding
(global-set-key (kbd "C-c i") 'clone-indirect-buffer)

;; view mode instead of read only mode
(global-set-key (kbd "C-x C-q") 'view-mode)

;; ----------------------------------------
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

;; ----------------------------------------
(defvar my-global-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-t" 'my-touch-left)
    map)
"The map containing the bindings of my own commands in the global map")

(define-key global-map "\C-\M-m" my-global-prefix-map)

;; ----------------------------------------
(defun my-get-defun-name ()
  "Puts in the kill ring the name of the current defun. Useful when taking notes
about the function."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (let ((defun-form (read (current-buffer))))
      (kill-new (symbol-name (cadr defun-form))))))

(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-t") 'my-get-defun-name)
  (define-key lisp-interaction-mode-map (kbd "C-c C-t") 'my-get-defun-name))

;; ----------------------------------------

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

;; ----------------------------------------
(global-set-key "\C-ha" 'apropos)

;; ----------------------------------------
(global-set-key "\C-x\C-n" nil)

;; ----------------------------------------
;; * registers
(set-register ?r "(region-beginning) (region-end)")
(set-register ?l "~/leng/")
(set-register ?e "~/.emacs.d/")

;; ----------------------------------------
(when (get-buffer "*scratch*")
  (kill-buffer "*scratch*")
  (find-file-noselect "/tmp/scratch.el"))

;; ----------------------------------------
(require 'undo-tree)
(global-undo-tree-mode)
