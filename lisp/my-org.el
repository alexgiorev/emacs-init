(require 'org)
(require 'ol)
(require 'org-id)
(require 'my-macs)
(require 'my-sched (concat user-emacs-directory "my-sched/my-sched.el"))
(require 'my-org-vars (concat user-emacs-directory "my-org-vars/my-org-vars.el"))

(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done nil)

;; ########################################
(setq-default org-startup-folded t)
(add-hook 'org-mode-hook (lambda nil (electric-indent-local-mode -1)))

;; ########################################

(defun my-org-isearch-headlines nil
  (interactive)
  (let ((isearch-filter-predicate
         (lambda (start end)
           (save-match-data
             (isearch-filter-visible start end)
             (save-excursion (goto-char start) (org-on-heading-p))))))
    (isearch-mode t nil nil t)))

(define-key org-mode-map (kbd "M-s e") 'my-org-isearch-headlines)

;; ########################################
;; widen to parent

(defun my-widen-to-parent nil
  (interactive)
  (goto-char (point-min)) ;; TODO: How to go to the root node?
  (push-mark)
  (widen)
  (outline-up-heading 1)
  (org-narrow-to-subtree))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c w p") 'my-widen-to-parent))

;; ########################################
(defun my-new-entry-today (&optional arg)
  "Assumes the top-level headlines are timestamps. Inserts a new child in
today's entry. If there is no entry for today, creates it. When called with ARG,
add a backlink as a BACKLINK property."
  (interactive "P")
  (let (today headtime backlink)
    (widen)
    (if arg (setq backlink (org-store-link 1)))
    (org-overview) ;; startup visibility
    ;; go to the last top-level heading
    (end-of-buffer) (while (org-up-heading-safe))
    (setq today (org-timestamp-from-time (current-time))
          headtime (org-timestamp-from-string
                    (org-get-heading :no-tags :no-todo :no-priority)))
    (if (not (equal (my-timestamp-ymd today) (my-timestamp-ymd headtime)))
        (progn (org-insert-heading-respect-content)
               (org-insert-time-stamp (current-time) nil 'inactive)
               (beginning-of-line)))
    ;; point is at the beginning of the today's headline
    (org-show-children)
    (progn
      (org-insert-heading-respect-content) (org-demote)
      (org-show-set-visibility 'canonical))
    (if backlink (org-entry-put (point) "BACKLINK" backlink))))

(defun my-timestamp-ymd (timestamp)
  "Returns a list (YEAR MONTH DAY) for an org timestamp object"
  (let ((plist (cadr timestamp)))
    (list (plist-get plist :year-start)
          (plist-get plist :month-start)
          (plist-get plist :day-start))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c n") 'my-new-entry-today))

;; ########################################
;; * yanking

(defun my-org-yank-list nil
  (interactive)
  (save-restriction
    (let ((start (point))
          (end (progn (yank) (point))))
      (narrow-to-region start end)
      (goto-char (point-min))
      (delete-blank-lines) (delete-blank-lines)
      (while (progn
               (delete-horizontal-space)
               (insert "- ")
               (forward-line)
               (not (eobp)))))))

(defun my-yank-from-pdf nil
  (interactive)
  (let (text)
    (with-temp-buffer
      (yank) (beginning-of-buffer)
      (while (re-search-forward "[-­‐]\n" nil t)
        (replace-match ""))
      (unfill-region (point-min) (point-max))
      (setq text (buffer-string)))
    (insert text)))

(defun my-yank-from-info nil
  (yank)
  (my-strip-region (region-beginning) (region-end))
  (unfill-region (region-beginning) (region-end)))

(defun my-strip-region (start end)
  "For each line in the region, strips beginning whitespace."
  (save-excursion
    (save-restriction
      (setq start (progn (goto-char start) (beginning-of-line) (point)))
      (setq end (progn (goto-char end) (end-of-line) (point)))
      (narrow-to-region start end)
      (goto-char start)
      (let (delete-start delete-end)
        (while (not (eobp))
          (setq delete-start (point))
          (skip-chars-forward "[[:blank:]]")
          (setq delete-end (point))
          (delete-region delete-start delete-end)
          (forward-line))))))

(defun my-yank-unfill nil
  (interactive)
  (let (text)
    (with-temp-buffer
      (yank) (unfill-region (point-min) (point-max))
      (setq text (buffer-string)))
    (insert text)))

(defun my-org-yank-unfill-elisp-comment nil
  (interactive)
  (let (text)
    (with-temp-buffer
      (emacs-lisp-mode)
      (yank) (uncomment-region (point-min) (point-max))
      (unfill-region (point-min) (point-max))
      (setq text (buffer-string)))
    (insert text)))

(defun my-org-yank-random-child nil
  (interactive)
  (my-org-insert-random-child)
  (if (org-kill-is-subtree-p)
      (org-paste-subtree)
    (yank) (org-back-to-heading))
  ;; hide subtree
  (outline-hide-subtree))

(defun my-org-yank-anki-cloze nil
  "Useful when I yank from Anki Cloze notes"
  (interactive)
  (let (text (cloze-re "{{c[0-9]+::\\(.*?\\)}}"))
    (with-temp-buffer
      (yank) (beginning-of-buffer)
      (while (re-search-forward cloze-re nil t)
        (replace-match (match-string 1)))
      (setq text (buffer-string)))
    (insert text)))

(defvar my-org-yank-map (make-sparse-keymap))
(progn
  (define-key my-org-yank-map "l" 'my-org-yank-list)
  (define-key my-org-yank-map "u" 'my-yank-unfill)
  (define-key my-org-yank-map "p" 'my-yank-from-pdf)
  (define-key my-org-yank-map "e" 'my-org-yank-unfill-elisp-comment)
  (define-key my-org-yank-map "r" 'my-org-yank-random-child)
  (define-key my-org-yank-map "z" 'my-org-yank-anki-cloze))
(define-key org-mode-map "\C-cy" my-org-yank-map)
(global-set-key "\C-\M-y" 'my-yank-unfill)

;; ########################################

(defun my-wrap-entry nil
  (interactive)
  (org-back-to-heading)
  (forward-line)
  (insert my-wrap-entry-title "\n")
  (forward-line -1)
  (org-insert-heading)
  (org-demote)
  (outline-hide-subtree))

(defvar my-wrap-entry-title
  "ENTRY_TEXT"
  "The title of the heading which is to wrap the entry of the current heading")

(with-eval-after-load 'org
  (define-key org-mode-map "\C-ch" 'my-wrap-entry))

;; ########################################

(defun my-org-insert-random-child nil
  (interactive)
  (org-back-to-heading)
  (let* ((parent-pos (point))
         (posns (cons parent-pos (my-org-child-positions)))
         (pos (nth (random (length posns)) posns))
         (org-blank-before-new-entry '((heading . nil))))
    (if (= pos parent-pos)
        (progn (org-insert-heading-respect-content)
               (org-demote))
      (goto-char pos)
      (org-insert-heading))))

(defun my-org-child-positions (&optional dont-reverse)
  "Returns a list of the positions of the children of the node at point.
By position it is meant the position of the beginning of the heading of the
node. If the order of the list is not important, setting DONT-REVERSE to t will
result in faster runtime."
  (save-excursion
    (let ((child-level (1+ (org-current-level)))
          (list-of-posns nil))
      (org-map-tree
       (lambda nil
         (if (= (org-current-level) child-level)
             (setq list-of-posns (cons (point) list-of-posns)))))
      (unless dont-reverse (reverse list-of-posns)))))

;; ########################################

(defun my-org-insert-merged-files (files)
  "Merge the org files FILES and insert into the current buffer.
To each file in FILES, there will be a top-level entry in the buffer whose
heading will be the name of the file. For each top-level entry, the body text
will be the file body text of the file, and the children will be the top-level
entries from the file."
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (org-mode)
      (dolist (file files)
        (insert-file-contents file)
        (insert "* " (file-name-base file) "\n")
        (if (not (org-on-heading-p)) (outline-next-heading))
        ;; at this point we are at the first heading that came from the file
        (org-map-region 'org-demote (point) (point-max))
        ;; make sure we end in newline
        (unless (= (char-before (point-max)) ?\n)
          (goto-char (point-max)) (insert-char ?\n))
        (append-to-buffer buffer (point-min) (point-max))
        (erase-buffer)))))

(defun my-dired-files-list nil
  "Returns a list of the absolute file names shown in the Dired buffer."
  (let ((list nil))
    (dired-map-dired-file-lines
     (lambda (file) (setq list (cons file list))))
    (reverse list)))

;; ########################################
(defun my-org-narrow-random-top-entry nil
  (interactive)
  (widen)
  (my-org-random-entry
   (lambda nil (= (funcall outline-level) 1)))
  (while (org-up-heading-safe))
  (org-narrow-to-subtree)
  (outline-hide-sublevels 2))

;; for the benefit of capture templates
(setq org-directory "~/notes")

;; Timer start/stop
(global-set-key (kbd "C-c C-x s")
                (lambda (prefix-arg)
                  (interactive "P")
                  (if prefix-arg
                      (funcall 'org-timer-stop)
                    (funcall 'org-timer-start))))

;; org-todo-keywords
;; Some TODO keywords are used to mark entry types. Such entries are _not_
;; tasks, so "TODO" keywords is a misnomer. Not sure it is a good idea to use
;; TODO keywords to mark the type of an entry. "Done" keywords serve just to
;; mark the type of the entry, whereas "TODO" keywords serve to attract may
;; attention as well.
(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE")
        (sequence "PROCESS" "|" "PROCESSED")
        (sequence "NEW" "|")
        (type "|" "TEMPDONE(-)")
        (type "|" "PASSIVE(s)")
        (type "|" "LIST" "HEAP")
        (type "|" "CLONE")
        (type "|" "DECISION(n)" "PAST_DECISION")
        (type "|" "DECL(e)" "FACT" "CONCEPT(c)" "SOURCE" "EXAMPLES" "TEMP")
        (type "QUESTION(q)" "|" "ANSWERED" "ANSWER(a)")
        (type "PROBLEM(p)" "|" "SOLVED" "SOLUTION(o)" "PROBLEM_DECL")
        (type "IDEA(i)" "|" "IDEA_DECL")
        (type "EXPLORE(x)" "EXPERIMENT" "ACTION" "HOOK" "LATER" "READ(r)" "|")))

;; so that level 2 entries are also considered when refiling
(setq org-refile-targets
      '((nil . (:maxlevel . 2))
        (nil . (:tag . "refileTarget"))
        ("~/notes/org" . (:level . 1))
        ("~/notes/leng/leng" . (:level . 1))
        ("~/notes/leng/heaps" . (:level . 1))
        ("~/notes/leng/blog" . (:level . 1))))

;; so that indentation is not changed when demoting/promoting
(setq org-adapt-indentation nil)

;; to randomize entries
(defun my-randomize-entries nil
  (interactive)
  (org-sort-entries nil ?f (lambda (&rest args) (random)))
  (org-cycle))

;; quick insertion of caption
(add-hook 'org-mode-hook
          (lambda nil
            (push (cons ?c "#+CAPTION: ")
                  register-alist)))

;;########################################
;; Capture

(define-key global-map (kbd "C-c c c") 'org-capture)
(setq org-capture-templates nil)

;; insert new source
(add-to-list
 'org-capture-templates
 '("s" "Learning source (article, book, etc.)"
   entry
   (file "leng/sources")
   "* %?\n:PROPERTIES:\n:URL: %x\n:DATES: %t\n:END:\n"))

;; to insert new english words on the fly
(add-to-list
 'org-capture-templates
 '("e" "Insert paragraph containing a new English word."
   plain
   (file+headline "leng/english" "Vocabulary")
   "%i" :empty-lines 1 :immediate-finish t))

(define-key global-map (kbd "C-c c e")
  (lambda nil (interactive) (org-capture nil "e")))

;; extracting into an arbitrary entry
(add-to-list
 'org-capture-templates
 '("x" "Extract marked element into target entry."
   plain
   (file+headline nil nil) ; set dynamically
   "%i" :empty-lines 1 :immediate-finish t))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c c x")
    (lambda nil (interactive) (org-capture nil "x"))))

(defun my-set-extract-target nil
  (interactive)
  (let ((new-target (list 'file+headline
                          (or (buffer-file-name)
                              (signal 'file-error
                                      "buffer file name is nil"))
                          (nth 4 (org-heading-components))))
        (entry (assoc "x" org-capture-templates)))
    (setcar (nthcdr 3 entry) new-target)))

;;########################################
(with-eval-after-load 'org
  (define-key org-mode-map "\C-a" 'org-beginning-of-line)
  (define-key org-mode-map "\C-e" 'org-end-of-line)
  (define-key org-mode-map "\C-k" 'org-kill-line))
(setq org-special-ctrl-a/e t)

;; random entry
(defun my-org-random-entry-from-point nil
  (interactive)
  (my-random-point)
  (org-back-to-heading))

(defun my-org-random-entry (&optional pred)
  "Move point to a random entry in the accessible region. When PRED is non-nil,
consider only the headings which match PRED as possible candidates. PRED is a
function with no arguments called with point at the beginning of the heading"
  (interactive)
  (let (posns)
    (org-map-region
     (if pred
         (lambda nil (when (funcall pred) (push (point) posns)))
       (lambda nil (push (point) posns)))
     (point-min) (point-max))
    (goto-char (my-randchoice posns))))

(setq-default org-fontify-done-headline nil)
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
(setq org-cycle-separator-lines 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; links

(setq org-id-link-to-org-use-id t)

(defun my-org-refile-link nil
  "Creates a link to the current entry and refiles it."
  (interactive)
  (save-excursion
    (let (link)
      (org-back-to-heading t)
      (setq link (org-store-link 1))
      (org-insert-heading)
      (insert link)
      (org-refile))))

(defun my-org-kill-link nil
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (kill-new (org-store-link 1))))

(defvar my-org-link-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-i") 'org-insert-link)
    (define-key map (kbd "C-s") 'org-store-link)
    (define-key map (kbd "C-n") 'org-next-link)
    (define-key map (kbd "C-p") 'org-previous-link)
    (define-key map (kbd "C-r") 'my-org-refile-link)
    (define-key map (kbd "C-k") 'my-org-kill-link)
    map))

;;; link bindings
(define-key global-map (kbd "C-c l") 'org-store-link)
(with-eval-after-load 'org
  ;; first make sure C-c C-l is not bound so that it can serve as a prefix
  (define-key org-mode-map (kbd "C-c C-l") nil)
  (define-key org-mode-map (kbd "C-c C-l") my-org-link-prefix-map))

;; ########################################
;; * source code

(defun my-org-yank-code (lang)
  (let (text)
    (with-temp-buffer
      (yank) (beginning-of-buffer)
      (insert (format "#+begin_src %s\n" lang))
      (end-of-buffer)
      (unless (= (char-before) ?\n)
        (insert "\n"))
      (insert "#+end_src")
      (setq text (buffer-string)))
    (save-excursion (insert text))))

(defun my-org-codify-region nil
  (interactive)
  (let (start end)
    (if (use-region-p)
        (progn (setq start (region-beginning) end (region-end))
          (goto-char start) (insert "~")
          (goto-char (1+ end)) (insert "~"))
      (insert "~~") (backward-char))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-M-c") 'my-org-codify-region))

(defun my-org-code (start end)
  (interactive "r")
  (if (not (region-active-p))
      (save-excursion
        (yank) (activate-mark) (org-babel-demarcate-block))
    (org-babel-demarcate-block)))

(defvar my-org-block-map (make-sparse-keymap))
(progn
  (define-key my-org-block-map "s" 'my-org-code)
  (define-key my-org-block-map "e"
    (lambda nil (interactive)
      (my-org-yank-code "elisp")))
  (define-key my-org-block-map "p"
    (lambda nil (interactive)
      (my-org-yank-code "python"))))
(define-key org-mode-map (kbd "C-c b") my-org-block-map)

;; ########################################

(defun my-org-paste-subtree-advice (old-func &rest args)
  "I don't like that the function sometimes inserts blank lines after it pastes
the subtree. This advice remembers the number of blank lines before the paste,
and if the `org-paste-subtree' inserted extra, they are deleted."
  (let ((empty-lines-before (my-count-preceding-empty-lines))
        empty-lines-after diff)
    (apply old-func args)
    (setq empty-lines-after (my-count-preceding-empty-lines)
          diff (- empty-lines-after empty-lines-before))
    (unless (= diff 0)
      (backward-char diff)
      (delete-char diff))))

(advice-add 'org-paste-subtree :around 'my-org-paste-subtree-advice)

;; ########################################

(defun my-org-meta-return-advice (FUN &optional arg)
  (if (equal arg '(64))
      (my-org-insert-random-child)
    (funcall FUN arg)))

(advice-add 'org-meta-return :around 'my-org-meta-return-advice)

;; ########################################

(defun my-org-collect-entries (pred)
  "Collect all entries which pass PRED and return them as a string.
PRED is a predicate function called at the beginning of each entry in the
buffer, except that if an entry passes PRED the search continues past the
subtree of the entry."
  (let ((buffer (current-buffer))
        temp-buffer)
    (with-temp-buffer
      (setq temp-buffer (current-buffer)) (set-buffer buffer)
      (save-excursion
        (beginning-of-buffer)
        (when (or (org-on-heading-p) (outline-next-heading))
          (while (progn
                   (if (save-excursion (funcall pred))
                       (progn
                         ;; Move the subtree to the temp buffer and position point
                         ;; at its end so that the search continues after it.
                         (let ((subtree
                                (buffer-substring
                                 (point)
                                 (progn (org-end-of-subtree t t) (point)))))
                           (with-current-buffer temp-buffer
                             (save-excursion (insert subtree))
                             (my-org-tree-set-level 1)
                             (end-of-buffer)))
                         ;; only continue the loop if on a heading after the
                         ;; subtree
                         (and (not (eobp)) (org-on-heading-p)))
                     (outline-next-heading))))))
        (with-current-buffer temp-buffer
          (buffer-string)))))

(defun my-org-has-TODO-descendant (kwd)
  "Returns t when a descendant of the current subtree has a todo keyword KWD"
  (save-excursion
    (org-back-to-heading t)
    (let ((regexp (format org-heading-keyword-regexp-format kwd))
          (end (save-excursion (org-end-of-subtree t t) (point))))
      (not (null (re-search-forward regexp end t))))))

;; ########################################
;; opening ID links should visit them in the same buffer

(with-eval-after-load 'org
  (setcdr (assoc 'file org-link-frame-setup)
          'find-file))

;; ########################################
;; my-org-ring

(defvar my-org-ring nil
  "A ring of org-mode entry positions which I want to use for myself.
Many commands use the org-mark-ring, but I want a ring that I am in total control of
and whose positions are always explictily set.")

(defun my-org-ring-empty nil
  (interactive)
  (while my-org-ring
    (my-org-ring-remove)))

(defun my-org-ring--check nil
  (if (not my-org-ring)
      (user-error "Ring is empty")))

(defun my-org-ring-push (&optional before)
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((marker (point-marker)))
      (put-text-property (point) (1+ (point)) :my-org-ring-marker marker)
      (if my-org-ring
          (setq my-org-ring (my-circlist-add my-org-ring marker before))
        (setq my-org-ring (list marker))
        (my-circlist-make my-org-ring)))))

(defun my-org-ring-jump nil
  (interactive)
  (let (jump? marker)
    (while (and my-org-ring (not jump?))
      (setq marker (car my-org-ring))
      (ignore-errors
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            (when (invisible-p (point))
              (org-show-set-visibility 'canonical))
            (when (eq marker (get-text-property (point) :my-org-ring-marker))
              (setq jump? t)))))
      (unless jump?
        (my-circlist-pop 'my-org-ring)))
    (if my-org-ring
        (progn
          (my-jump-to-marker marker)
          (when (org-invisible-p)
            (org-show-set-visibility 'canonical)))
      (my-org-ring--check))))

(defun my-org-ring-next nil
  (interactive)
  (my-org-ring--check)
  (setq my-org-ring (cdr my-org-ring))
  (my-org-ring-jump))

(defun my-org-ring-prev nil
  (interactive)
  (my-org-ring--check)
  (setq my-org-ring (my-circlist-prev my-org-ring))
  (my-org-ring-jump))

(defun my-org-ring-remove (&optional jump)
  (interactive "P")
  (my-org-ring--check)
  (my-org-ring--remove)
  (when jump (my-org-ring-jump)))

(defun my-org-ring--remove nil
  (let ((marker (my-circlist-pop 'my-org-ring)))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (while (text-property-search-forward :my-org-ring-marker marker)
          (remove-text-properties
           (1- (point)) (point) (list :my-org-ring-marker nil)))
        (set-marker marker nil nil)))))

(defun my-org-ring-todo-tree nil
  "Initialize the ring with the TODO nodes in the tree at point"
  (interactive)
  (my-org-ring-empty)
  (save-excursion
    (org-map-tree
     (lambda nil
       (if (org-entry-is-todo-p) (my-org-ring-push)))))
  (setq my-org-ring (cdr my-org-ring)))

(defvar my-org-ring-map (make-sparse-keymap))
(progn 
  (define-key my-org-ring-map "e" 'my-org-ring-empty)
  (define-key my-org-ring-map " " 'my-org-ring-push)
  (define-key my-org-ring-map "p" 'my-org-ring-prev)
  (define-key my-org-ring-map "n" 'my-org-ring-next)
  (define-key my-org-ring-map "j" 'my-org-ring-jump)
  (define-key my-org-ring-map "r" 'my-org-ring-remove)
  (define-key my-org-ring-map "t" 'my-org-ring-todo-tree))

(define-key org-mode-map "\C-cr" my-org-ring-map)

;; ########################################

(defun my-org-restart-preserve-visibility nil
  "Restart org-mode but don't change visibility"
  (interactive)
  (org-with-wide-buffer
   (let* ((overlay-attrs
           (mapcar (lambda (overlay)
                     (let (invisible)
                       (when (and (setq invisible
                                        (overlay-get overlay 'invisible))
                                  (invisible-p invisible))
                         (cons (cons (overlay-start overlay)
                                     (overlay-end overlay))
                               invisible))))
                     (overlays-in (point-min) (point-max)))))
     (org-mode-restart)
     (org-show-all)
     (dolist (attrs overlay-attrs)
       (pcase-let ((`((,start . ,end) . ,invisible) attrs))
         (org-flag-region start end t invisible))))))

;; ########################################

(defun my-org-id-get-tree (eid)
  "Returns as a string the entry having id EID or nil if no such entry"
  (let ((location (org-id-find eid)))
    (when location
      (with-current-buffer (get-file-buffer (car location))
        (org-with-wide-buffer
         (goto-char (cdr location))
         (org-back-to-heading t)
         (my-org-tree-text :no-properties))))))

;; ########################################
;; * clones

(defvar my-org-id-link-re
  (let* ((id-re "id:\\(.*?\\)")
         (link-re (format "%s\\|\\[\\[%s\\]\\(?:\\[.*?\\]\\)\\]" id-re id-re)))
    link-re)
  "A regexp which matches an ID link to an entry. The actual ID is in group 1 or 2.
 To get it run (or (match-string 1) (match-string 2))")

(defun my-org-clone-fetch (eid)
  "Insert as a sibling to the current tree the tree whose ID is EID"
  (let ((tree (my-org-id-get-tree eid)))
    (unless tree
      (error (format "No tree having ID \"%s\"" eid)))
    (org-insert-heading-respect-content t)
    (org-paste-subtree nil tree)
    (org-entry-delete nil "ID") (org-entry-put nil "ORIG_ID" eid)
    (org-entry-put nil "CLONE" "t")
    (save-excursion
      (my-org-tree-filter
       (lambda nil
         (let ((id (org-entry-get nil "ID"))
               (clone-p (org-entry-get nil "CLONE")))
           ;; clones are deleted because the `unless' will return nil
           (unless clone-p
             (when id
               (org-entry-delete nil "ID")
               (org-entry-put nil "ORIG_ID" id))
             ;; make sure to not delete the subtree
             :dont-delete)))
       :skip-root))
    ;; put in CHILDREN view
    (org-flag-subtree t) (org-cycle)))

(defsubst my-org-clone-ref-p nil
  "When the current entry is a CLONE, return the ID of the original. Otherwise, return nil"
  (save-excursion
    (org-back-to-heading t)
    (when (and (string= (org-get-todo-state) "CLONE")
               (not (null (re-search-forward my-org-id-link-re
                                             (line-end-position)
                                             t))))
      (or (match-string 1) (match-string 2)))))

(defun my-org-clone-pull nil
  "Insert the text of the original entry as a sibling of the current one.
Point must be on a CLONE entry for this to work."
  (interactive)
  (let (id)
    (unless (setq id (my-org-clone-ref-p))
      (error "Point must be on a CLONE entry"))
    (my-org-clone-fetch id)))

(defun my-org-clone-push-single nil
  "Replace the original entry with the text of the clone at point"
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let (orig-id orig-location orig-level
          clone-text clone-visibility)
      (unless (org-entry-get nil "CLONE")
        (error "Current node is not a clone"))
      (unless (setq orig-id (org-entry-get nil "ORIG_ID"))
        (error "Missing original ID"))
      (setq orig-location (org-id-find orig-id)
            clone-text (my-org-tree-text :no-properties)
            clone-visibility (my-org-tree-get-visibility))
      (with-current-buffer (get-file-buffer (car orig-location))
        (org-with-wide-buffer
         (goto-char (cdr orig-location)) (org-back-to-heading t)
         (setq orig-level (funcall outline-level))
         (narrow-to-region
          (point) (my-org-tree-end-pos t t))
         (delete-region (point-min) (point-max))
         (save-excursion (insert clone-text))
         (my-org-tree-set-visibility clone-visibility)
         (my-org-tree-set-level orig-level)
         ;; process the root
         (org-entry-delete nil "ORIG_ID") (org-entry-put nil "ID" orig-id)
         (org-entry-delete nil "CLONE")
         (my-org-tree-filter
          (lambda nil
            (let (orig-id)
              ;; Clones will be deleted because the `unless' will return nil
              (unless (org-entry-get nil "CLONE")
                (when (setq orig-id (org-entry-get nil "ORIG_ID"))
                  (org-entry-delete nil "ORIG_ID")
                  (org-entry-put nil "ID" orig-id))
                :dont-delete)))))))))

;; ########################################
;; * misc

(defsubst my-org-move-to-title nil
  "Move to the beginning of the title of the current heading, or just to the
beginning of the heading when it has no title."
  (org-back-to-heading t)
  (looking-at org-complex-heading-regexp)
  (when (match-string 4) (goto-char (match-beginning 4))))

;; ** trees

(defun my-org-tree-set-level (level)
  "Promote/demote the subtree at point so that its root has level LEVEL"
  (save-excursion
    (org-back-to-heading t)
    (let* ((tree-level (funcall outline-level))
           (diff (- level tree-level))
           ;; READ If the diff is zero, this computation is wasteful
           (set-level-func (if (> diff 0)
                               (lambda nil (insert (make-string diff ?*)))
                             (lambda nil (delete-char (- diff))))))
      (when (/= diff 0)
        (org-map-tree set-level-func)))))

(defsubst my-org-tree-text (&optional no-properties)
  (save-excursion
    (org-back-to-heading t)
    (if no-properties
        (buffer-substring-no-properties
         (point) (progn (org-end-of-subtree t t) (point)))
      (buffer-substring
         (point) (progn (org-end-of-subtree t t) (point))))))

(defsubst my-org-tree-end-pos (&rest args)
  (save-excursion (apply 'org-end-of-subtree args) (point)))

(defsubst my-org-tree-delete nil
  (save-excursion
    (org-back-to-heading t)
    (delete-region (point) (my-org-tree-end-pos t t))))

(defun my-org-tree-filter (pred &optional skip-root)
  "Keep in the tree at point only the nodes which satisfy PRED, which is a
function called with point at the beginning of a node. The nodes are traversed
in order and if one doesn't satisfy PRED, the whole subtree of the node is
deleted. This means that if the root doesn't satisfy PRED, the whole tree will
be deleted. When optional argument SKIP-ROOT is non-nil, begin the traversal
from the first child of the tree instead of at its root."
  (save-excursion
    (org-back-to-heading)
    (let ((end (make-marker)))
      (set-marker end (my-org-tree-end-pos t t))
      (when skip-root (outline-next-heading))
      (while (< (point) end)
        (if (funcall pred)
            (outline-next-heading)
          (my-org-tree-delete)))
      (set-marker end nil nil))))

(defun my-org-get-visibility (region-start region-end)
  "Return a list of triples (START END SPEC) which describes the visibility of
the region delimited by REGION-START and REGION-END. The START and END values in
each triple are relative to REGION-START and are zero-indexed. SPEC is a value
found under the `invisible' property, or nil when the region is visible there."
  (save-excursion
    (let ((region-start (point))
          (region-end (my-org-tree-end-pos))
          visibility start end spec)
      (while (< (point) region-end)
        (setq start (- (point) region-start)
              spec (get-char-property (point) 'invisible))
        (goto-char (next-single-char-property-change
                    (point) 'invisible nil region-end))
        (setq end (- (point) region-start))
        (push (list start end spec) visibility))
      (reverse visibility))))

(defun my-org-tree-get-visibility nil
  (let (start end)
    (save-excursion
      (org-back-to-heading t) (setq start (point))
      (org-end-of-subtree t t) (setq end (point)))
    (my-org-get-visibility start end)))

(defun my-org-tree-set-visibility (visibility)
  "Set the visibility of the current entry to match VISIBILITY (whose format you
  can see in `my-org-get-visibility'. This function assumes that VISIBILITY
  was extracted from an entry with the exact same text."
  (save-excursion
    (org-back-to-heading t)
    (let ((tree-start (point)))
      (while visibility
        (pcase-let ((`(,start ,end ,spec) (car visibility)))
          (if spec
              (org-flag-region
               (+ tree-start start) (+ tree-start end)
               t spec)
            (my-org-make-visible
             (+ tree-start start) (+ tree-start end))))
        (setq visibility (cdr visibility))))))

(defun my-org-make-visible (start end)
  "Make the region delimited by START and END visible regardless of what
`invisible' values are used."
  (interactive "r")
  (save-excursion
    (let ((overlays (overlays-in start end))
          spec)
      (dolist (overlay overlays)
        (when (invisible-p (setq spec (overlay-get overlay 'invisible)))
          (org-flag-region (max start (overlay-start overlay))
                           (min end (overlay-end overlay))
                           nil spec))))))

(defun my-org-id-get-ids (files)
  "Return a list of all IDs inside links in FILES"
  (let ((ids (make-hash-table :test 'equal)))
    (my-org-run-in-files
     (lambda nil
       (org-with-wide-buffer
        (beginning-of-buffer)
        (while (re-search-forward my-org-id-link-re nil t)
          (puthash (or (match-string-no-properties 1)
                       (match-string-no-properties 2))
                   t ids)))
       files)
     (hash-table-keys ids))))

(defun my-org-run-in-files (func files &optional save)
  "Run FUNC in each file in FILES. Visit the files with org-mode as the
major-mode. If optional argument SAVE is non-nil, save the files after running
FUNC."
  (let (buffer was-visited)
    (dolist (file files)
      (unless (file-directory-p file)
        (setq was-visited nil)
        (unless (setq was-visited (setq buffer (get-file-buffer file)))
          (setq buffer (find-file-noselect file)))
        (when (not (eq major-mode 'org-mode))
          (org-mode))
        (with-current-buffer buffer
          (save-excursion
           (funcall func) (save-buffer)))
        (unless was-visited
          (kill-buffer buffer))))))


;; ########################################
;; * tempdone

(defun my-org-tempdone-after-state-change nil
  (if (string= org-state "TEMPDONE")
      (unless (string= org-last-state "TEMPDONE")
        (org-entry-put (point) "TEMPDONE_UNDO" org-last-state)
        (org-hide-entry))
    (when (string= org-last-state "TEMPDONE")
      (org-entry-delete (point) "TEMPDONE_UNDO"))))

(defun my-org-tempdone (&optional days)
  (let ((state (org-get-todo-state))
        (org-after-todo-state-change-hook
         (remq 'my-org-tempdone-after-state-change
               org-after-todo-state-change-hook))
        (day (and days (+ (my-time-today) days))))
    (unless (string= state "TEMPDONE")
      (org-todo "TEMPDONE")
      (org-entry-put nil "TEMPDONE_UNDO" state)
      (when day
        (org-entry-put nil "TEMPDONE_UNDO_DAY" (number-to-string day)))      
      (org-hide-entry))))

(defun my-org-tempdone-days (days)
  (interactive "nDays: ")
  (my-org-tempdone days))

(defun my-org-tempdone-undo-buffer nil
  "Go through the accessible portion of the current buffer and undo the TEMDONE entries"
  (interactive)
  (my-org-tempdone-undo-region (point-min) (point-max)))

(defun my-org-tempdone-undo-region (start end)
  (interactive "r")
  (org-map-region
   'my-org-tempdone-undo start end)
  (deactivate-mark))

(defun my-org-tempdone-undo (&optional force)
  "If the current entry is a TEMPDONE, undo it. If a TEMPDONE date is present,
don't undo unless it refers to today or a day that has passed. If FORCE is
non-nil, undo regardless of date."
  (interactive)
  (let (old day)
    (when (string= (org-get-todo-state) "TEMPDONE")
      (setq day (ignore-errors (string-to-number (org-entry-get nil "TEMPDONE_UNDO_DAY"))))
      (when (or force (not day) (<= day (my-time-today)))
        (setq old (org-entry-get nil "TEMPDONE_UNDO"))
        (unless old
          (error "TEMPDONE entry missing TEMPDONE_UNDO property"))
        (org-entry-delete nil "TEMPDONE_UNDO")
        (org-entry-delete nil "TEMPDONE_UNDO_DAY")
        (org-todo old)))))

(add-hook 'org-after-todo-state-change-hook
          'my-org-tempdone-after-state-change)

(add-hook 'org-mode-hook
          'my-org-tempdone-undo-buffer)

(defvar my-org-todo-map (make-sparse-keymap))
(progn
  (define-key my-org-todo-map "d" 'my-org-tempdone-days))
(define-key org-mode-map "\C-ct" my-org-todo-map)

;;########################################
(provide 'my-org)
