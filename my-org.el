(require 'org)
(require 'ol)
(require 'org-id)
(require 'my-macs "~/.emacs.d/my-macs")
(require 'my-sched "~/.emacs.d/my-sched/my-sched")

(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done nil)

;; ----------------------------------------
(setq-default org-startup-folded t)
(add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))

;; ----------------------------------------

(defun my-isearch-headlines ()
  (interactive)
  (let ((isearch-filter-predicate 'my-heading-filter-predicate))
    (isearch-mode t nil nil t)))

(defun my-heading-filter-predicate (start end)
  (save-match-data
    (and (isearch-filter-visible start end) ;; to reveal the invisible
         (my-same-line start end) (my-on-headline start)
         (progn (message "after: %i" (match-beginning 0)) t))))

(defun my-same-line (pos1 pos2)
  (save-excursion
    (= (progn (goto-char pos1) (line-beginning-position))
       (progn (goto-char pos2) (line-beginning-position)))))

(defun my-on-headline (pos)
  (my-same-line pos (save-excursion (org-back-to-heading t) (point))))

;; ----------------------------------------
;; widen to parent

(defun my-widen-to-parent ()
  (interactive)
  (goto-char (point-min)) ;; TODO: How to go to the root node?
  (push-mark)
  (widen)
  (outline-up-heading 1)
  (org-narrow-to-subtree))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c w p") 'my-widen-to-parent))

;; ----------------------------------------
(defun my-new-entry-today (&optional arg)
  "Assumes the top-level headlines are timestamps. Inserts a new child in
today's entry. If there is no entry for today, creates it. When called with ARG,
add a backlink as a BACKLINK property."
  (interactive "P")
  (let (today headtime backlink)
    (widen)
    (if arg (setq backlink (org-store-link 1)))
    (org-overview) ;; startup visibility
    (my-goto-last-top-heading)
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

(defsubst my-goto-last-top-heading ()
  "Moves point to the beginning of the last heading at level 1"
  (end-of-buffer)
  (while (org-up-heading-safe)))

(defun my-timestamp-ymd (timestamp)
  "Returns a list (YEAR MONTH DAY) for an org timestamp object"
  (let ((plist (cadr timestamp)))
    (list (plist-get plist :year-start)
          (plist-get plist :month-start)
          (plist-get plist :day-start))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c n") 'my-new-entry-today))

;; ----------------------------------------
(defun my-org-paste-list ()
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

(with-eval-after-load 'org
  (define-key org-mode-map "\C-cyl" 'my-org-paste-list))

;; ----------------------------------------

(defun my-wrap-entry ()
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

;; ----------------------------------------

(defun my-org-paste-random-child ()
  (interactive)
  (my-org-insert-random-child)
  (if (org-kill-is-subtree-p)
      (org-paste-subtree)
    (yank) (org-back-to-heading))
  ;; hide subtree
  (outline-hide-subtree))

(defun my-org-insert-random-child ()
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
       (lambda ()
         (if (= (org-current-level) child-level)
             (setq list-of-posns (cons (point) list-of-posns)))))
      (unless dont-reverse (reverse list-of-posns)))))

(with-eval-after-load 'org
  (define-key org-mode-map "\C-cyr" 'my-org-paste-random-child))

;; ----------------------------------------

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

(defun my-dired-files-list ()
  "Returns a list of the absolute file names shown in the Dired buffer."
  (let ((list nil))
    (dired-map-dired-file-lines
     (lambda (file) (setq list (cons file list))))
    (reverse list)))

;; ----------------------------------------
(defun my-org-narrow-random-top-entry ()
  (interactive)
  (widen)
  (my-random-entry)
  ;; goto root
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
        (sequence "PROCESS" "|" "TEMPDONE" "PROCESSED")
        (sequence "NEW" "|")
        (type "|" "PASSIVE(s)")
        (type "|" "LIST" "HEAP")
        (type "|" "CLONE")
        (type "|" "DECISION" "PAST_DECISION")
        (type "|" "DECL(e)" "FACT" "CONCEPT(c)" "SOURCE" "EXAMPLES" "TEMP")
        (type "QUESTION(q)" "|" "ANSWERED" "ANSWER(a)")
        (type "PROBLEM(p)" "|" "SOLVED" "SOLUTION(o)" "PROBLEM_DEFERRED")
        (type "EXPLORE" "EXPERIMENT" "ACTION" "HOOK" "LATER" "IDEA(i)" "READ(r)" "|")))

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
(defun my-randomize-entries ()
  (interactive)
  (org-sort-entries nil ?f (lambda (&rest args) (random)))
  (org-cycle))

;; quick insertion of caption
(add-hook 'org-mode-hook
          (lambda ()
            (push (cons ?c "#+CAPTION: ")
                  register-alist)))

;;----------------------------------------
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
  (lambda () (interactive) (org-capture nil "e")))

;; extracting into an arbitrary entry
(add-to-list
 'org-capture-templates
 '("x" "Extract marked element into target entry."
   plain
   (file+headline nil nil) ; set dynamically
   "%i" :empty-lines 1 :immediate-finish t))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c c x")
    (lambda () (interactive) (org-capture nil "x"))))

(defun my-set-extract-target ()
  (interactive)
  (let ((new-target (list 'file+headline
                          (or (buffer-file-name)
                              (signal 'file-error
                                      "buffer file name is nil"))
                          (nth 4 (org-heading-components))))
        (entry (assoc "x" org-capture-templates)))
    (setcar (nthcdr 3 entry) new-target)))

;;----------------------------------------
(with-eval-after-load 'org
  (define-key org-mode-map "\C-a" 'org-beginning-of-line)
  (define-key org-mode-map "\C-e" 'org-end-of-line)
  (define-key org-mode-map "\C-k" 'org-kill-line))
(setq org-special-ctrl-a/e t)

;;----------------------------------------
;; yanking from special sources

(defun my-yank-from-pdf ()
  (yank)
  (save-excursion
    (let ((end (region-end)))
      (goto-char (region-beginning))
      (while (re-search-forward "[-­‐]\n" end t)
        (replace-match ""))))
  (unfill-region (region-beginning) (region-end)))

(defun my-yank-from-info ()
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

(setq my-cloze-regexp "{{c[[:digit:]]*::\\(.*?\\)\\(?:::\\(.*?\\)\\)?}}")
(defun my-strip-cloze ()
  (interactive)
  (let (beginning end)
    (yank)
    ;; region beginning is before the yanked text
    (setq beginning (region-beginning) end (region-end))
    (goto-char beginning)
    (while (re-search-forward my-cloze-regexp end nil)
      (replace-match (match-string 1)))
    (goto-char end)))

(defun my-yank ()
  (interactive)
  (funcall my-yank-function))

(defun my-yank-kill ()
  "This is useful to process text and then paste it somewhere else, (e.g. Anki)"
  (interactive)
  (my-yank)
  (kill-region (region-beginning) (region-end)))

(setq my-yank-function 'my-yank-from-pdf)
(with-eval-after-load 'org
  (define-key org-mode-map "\C-\M-y" 'my-yank)
  (define-key org-mode-map "\C-\M-g" 'my-yank-kill))

;; random entry
(defun my-random-entry ()
  (interactive)
  (my-random-point)
  (org-back-to-heading))

(defun my-random-line ()
  (interactive)
  (my-random-point)
  (beginning-of-line))

(defun my-random-point ()
  (interactive)
  (goto-char (+ (point-min)
                (1+ (random (1+ (- (point-max) (point-min))))))))

(setq-default org-fontify-done-headline nil)
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
(setq org-cycle-separator-lines 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; links

(setq org-id-link-to-org-use-id t)

(defun my-org-refile-link ()
  "Creates a link to the current entry and refiles it."
  (interactive)
  (save-excursion
    (let (link)
      (org-back-to-heading t)
      (setq link (org-store-link 1))
      (org-insert-heading)
      (insert link)
      (org-refile))))

(defun my-org-kill-link ()
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-org-codify-region (start end)
  (interactive "r")
  (if (region-active-p)
      (progn (goto-char start) (insert "~")
             (goto-char (1+ end)) (insert "~"))
    (insert "~~") (backward-char)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-M-c") 'my-org-codify-region))

(defun my-org-code (start end)
  (interactive "r")
  (if (not (region-active-p))
      (save-excursion
        (yank) (activate-mark) (org-babel-demarcate-block))
    (org-babel-demarcate-block)))

(with-eval-after-load
    (define-key org-mode-map (kbd "C-c b s") 'my-org-code))

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

;; ----------------------------------------

(defun my-org-meta-return-advice (FUN &optional arg)
  (if (equal arg '(64))
      (my-org-insert-random-child)
    (funcall FUN arg)))

(advice-add 'org-meta-return :around 'my-org-meta-return-advice)

;; ----------------------------------------

(defun my-org-set-subtree-level (level)
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
                             (my-org-set-subtree-level 1)
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

;; ----------------------------------------
;; opening ID links should visit them in the same buffer

(with-eval-after-load 'org
  (setcdr (assoc 'file org-link-frame-setup)
          'find-file))

;; ----------------------------------------
;; my-org-ring

(defvar my-org-ring nil
  "A ring of org-mode entry positions which I want to use for myself.
Many commands use the org-mark-ring, but I want a ring that I am in total control of
and whose positions are always explictily set.")

(defun my-org-ring-empty nil
  (interactive)
  (setq my-org-ring nil))

(defun my-org-ring--check nil
  (if (not my-org-ring)
      (user-error "Ring is empty")))

(defun my-org-ring-push nil
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let ((marker (point-marker)))
      (put-text-property (point) (1+ (point)) :my-org-ring-marker marker)
      (if my-org-ring
          (setq my-org-ring (my-circlist-add-after my-org-ring marker))
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

(defun my-org-ring-remove nil
  (interactive)
  (my-org-ring--check)
  (my-circlist-pop 'my-org-ring))

(defvar my-org-ring-map)
(setq my-org-ring-map
      (let ((map (make-sparse-keymap)))
        (define-key map "e" 'my-org-ring-empty)
        (define-key map " " 'my-org-ring-push)
        (define-key map "p" 'my-org-ring-prev)
        (define-key map "n" 'my-org-ring-next)
        (define-key map "j" 'my-org-ring-jump)
        (define-key map "r" 'my-org-ring-remove)
        map))

(define-key org-mode-map "\C-cr" my-org-ring-map)

;; ----------------------------------------

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

;; ----------------------------------------

(defun my-org-id-get-entry (eid)
  "Returns as a string the entry having id EID or nil if no such entry"
  (let ((location (org-id-find eid)))
    (when location
      (with-current-buffer (get-file-buffer (car location))
        (org-with-wide-buffer
         (goto-char (cdr location))
         (org-back-to-heading t)
         (buffer-substring-no-properties
          (point) (org-end-of-subtree t t)))))))

;; ----------------------------------------
;; clones

(defun my-org-clone-link nil
  "Inserts a copy of the linked entry as a sibling of the current one.
 Works only for ID links to org-mode entries."
  (interactive)
  (let* ((id-re "id:\\(.*?\\)")
         (link-re (format "%s\\|\\[\\[%s\\]\\(?:\\[.*?\\]\\)\\]" id-re id-re))
         context link-obj-plist id entry)
    (unless (org-in-regexp link-re)
      (error "Point is not on an ID link"))
    (setq id (or (match-string 1) (match-string 2)))
    (setq entry (my-org-id-get-entry id))
    (unless entry
      (error (format "No entry having ID \"%s\"" id))))
    (org-insert-heading-respect-content t)
    (org-paste-subtree nil entry)
    (save-excursion
      (org-map-tree
       (lambda nil
         (when (setq id (org-entry-get (point) "ID"))
           (org-delete-property "ID")
           (org-entry-put (point) "ORIG_ID" id)))))
    (org-flag-subtree t) (org-cycle))

(defun my-org-clone-fetch nil
  "Insert the text of the original entry as a sibling of the current one.
Point must be on a CLONE entry for this to work."
  (interactive)
  (unless (string= (org-get-todo-state) "CLONE")
    (error "Point must be on a CLONE entry"))
  (my-org-move-to-title)
  (my-org-clone-linked))

;; ----------------------------------------
;; * misc

(defsubst my-org-move-to-title nil
  "Move to the beginning of the title of the current heading, or just to the
beginning of the heading when it has no title."
  (org-back-to-heading t)
  (looking-at org-complex-heading-regexp)
  (when (match-string 4) (goto-char (match-beginning 4))))
  