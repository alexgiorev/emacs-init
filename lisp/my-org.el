(require 'org)
(require 'ol)
(require 'org-id)
(require 'my-macs)
(require 'sched (concat (file-name-directory load-file-name) "sched/sched.el"))
(require 'my-org-vars (concat (file-name-directory load-file-name) "my-org-vars/my-org-vars.el"))

;;════════════════════════════════════════════════════════════
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

;;════════════════════════════════════════════════════════════
;; my date format

(defsubst my-org-date--goto-first nil
  (beginning-of-buffer)
  (let ((found-p
         (re-search-forward
          (concat "^\\* " org-ts-regexp-inactive) nil t)))
    (beginning-of-line)
    found-p))

(defun my-org-date--find-create (date-string)
  "Puts point on the top-level heading corresponding to DATE-STRING"
  (if (not (my-org-date--goto-first))
      (progn (ensure-newline)
             (insert "* " date-string "\n")
             (beginning-of-line 0))
    (loop
      (let ((title (org-get-heading t t t t)))
        (cond ((string= date-string title) (beginning-of-line) (end-loop))
              ((string< date-string title)
               (insert "* " date-string "\n") (beginning-of-line 0)
               (end-loop))
              (t (unless (org-goto-sibling)
                   (end-of-buffer) (ensure-newline)
                   (insert "* " date-string "\n") (beginning-of-line 0)
                   (end-loop))))))))

(defun my-org-date-send (file)
  "Assumes the current buffer is in my date format. Sends the entry at point
into a different file which should also have the date format"
  (interactive "fFile: ")
  (save-excursion
    (org-back-to-heading)
    (unless (= 2 (funcall outline-level))
      (user-error "Current heading not level 2"))
    (let ((buffer (current-buffer))
          (date-string (save-excursion
                         (org-up-heading-safe) (org-get-heading t t t t)))
          (tree (my-org-tree-text :no-props)))
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (my-org-date--find-create date-string)
         (org-end-of-subtree t t) (ensure-newline)
         (insert tree)))
      (my-org-tree-delete))))

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

;;════════════════════════════════════════════════════════════
;; * yanking & saving/killing

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
      (beginning-of-buffer)
      (while (re-search-forward
              "\\(ﬀ\\|\\<Th\\>\\|ﬁ\\|ﬂ\\|ﬃ\\|fi\\|fl\\) "
              nil t)
        (replace-match (match-string 1)))
      (setq text (buffer-string)))
    (push-mark) (insert text)))

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
    (push-mark) (insert text)))

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

(defun my-org-yank-_ nil
  (interactive)
  (let (string)
    (with-temp-buffer
      (yank)
      (downcase-region (point-min) (point-max))
      (beginning-of-buffer)
      (while (re-search-forward "\\s-+" nil t)
        (replace-match "_"))
      (setq string (buffer-string)))
    (push-mark)
    (insert string)))

(defun my-org-save-no-links (start end)
  (interactive "r")
  (kill-new (my-org-strip-links (buffer-substring start end)))
  (deactivate-mark))

(defvar my-org-yank-map (make-sparse-keymap))
(progn
  (define-key my-org-yank-map "l" 'my-org-yank-list)
  (define-key my-org-yank-map "u" 'my-yank-unfill)
  (define-key my-org-yank-map "p" 'my-yank-from-pdf)
  (define-key my-org-yank-map "e" 'my-org-yank-unfill-elisp-comment)
  (define-key my-org-yank-map "r" 'my-org-yank-random-child)
  (define-key my-org-yank-map "z" 'my-org-yank-anki-cloze)
  (define-key my-org-yank-map "i" 'my-org-yank-image)
  (define-key my-org-yank-map "_" 'my-org-yank-_)
  (define-key my-org-yank-map "[" 'my-org-save-no-links))
(define-key org-mode-map "\C-cy" my-org-yank-map)
(global-set-key "\C-\M-y" 'my-yank-unfill)

;;════════════════════════════════════════════════════════════

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
  "══════════════════════════════"
  "The title of the heading which is to wrap the entry of the current heading")

(with-eval-after-load 'org
  (define-key org-mode-map "\C-ch" 'my-wrap-entry))

;;════════════════════════════════════════════════════════════

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

;;════════════════════════════════════════════════════════════

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

;;════════════════════════════════════════════════════════════
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
        (type "CONJECTURE(j)" "|" "CONJECTURE_D")
        (type "|" "DECL(e)" "CONNECTION" "FACT" "CONCEPT(c)" "SOURCE" "EXAMPLES" "TEMP")
        (type "QUESTION(q)" "|" "ANSWERED" "ANSWER(a)")
        (type "PROBLEM(p)" "|" "SOLVED" "PROBLEM_D" "SOLUTION(o)")
        (type "UNDERSTAND(u)" "|" "UNDERSTOOD")
        (type "GOAL(g)" "|" "GOAL_D")
        (type "EXPLAIN(l)" "|" "EXPLAINED")
        (type "IDEA(i)" "|" "IDEA_DECL")
        (type "READ(r)" "READ_L" "|" "READ_D")
        (type "EXPLORE(x)" "ANKIFY(y)" "CONTINUE"
              "EXPERIMENT" "ACTION" "HOOK" "LATER" "|" "FUN")))

;; so that level 2 entries are also considered when refiling
(setq org-refile-targets
      '((nil . (:maxlevel . 2))
        (nil . (:tag . "refile_target"))
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

;;════════════════════════════════════════════════════════════
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

;;════════════════════════════════════════════════════════════
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
;;════════════════════════════════════════════════════════════
;; links

(setq org-id-link-to-org-use-id t)

(defun my-org-get-link (&optional components)
  (if current-prefix-arg
      (let (custom-id link desc)
        (unless (setq custom-id (org-entry-get nil "CUSTOM_ID"))
          (org-entry-put
           nil "CUSTOM_ID" (setq custom-id (my-org-next-custom-id))))
        (setq link (concat "file:" (buffer-file-name (buffer-base-buffer)) "::#" custom-id)
              desc (my-org-strip-links (org-get-heading t t t t)))
        (if components (list link desc) (format "[[%s][%s]]" link desc)))
    (org-store-link 1)))

(defun my-org-store-link (arg)
  (interactive "P")
  (if arg
      (push (my-org-get-link t) org-stored-links)
    (org-store-link arg t)))

(defun my-org-custom-id-from-title nil
  (let ((title (org-get-heading t t t t)))
    (with-temp-buffer
      (insert title) (beginning-of-buffer)
      (save-excursion (while (re-search-forward " +" nil t) (replace-match "-")))
      (save-excursion (while (re-search-forward "[^a-zA-Z-]" nil t) (replace-match "")))
      (downcase-region (point-min) (point-max))
      (insert "CUSTOM_ID_")
      (buffer-substring-no-properties (point-min) (point-max)))))

(pvars-add 'my-org-custom-id)
(unless (boundp 'my-org-custom-id)
  (setq my-org-custom-id 0))
(defun my-org-next-custom-id nil
  (prog1 (number-to-string my-org-custom-id)
    (cl-incf my-org-custom-id)))

(defun my-org-refile-link nil
  "Creates a link to the current entry and refiles it."
  (interactive)
  (save-excursion
    (let (link)
      (org-back-to-heading t)
      (setq link (my-org-get-link))
      (org-insert-heading)
      (insert link)
      (org-refile))))

(defun my-org-kill-link nil
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (kill-new (my-org-get-link))))

(defun my-org-link-file (file)
  (interactive "f")
  (let ((name (file-name-nondirectory file)))
    (insert (format "[[file:%s][%s]]" file name))))

(defvar my-org-link-prefix-map (make-sparse-keymap))
(progn
  (define-key my-org-link-prefix-map (kbd "C-i") 'org-insert-link)
  (define-key my-org-link-prefix-map (kbd "C-s") 'my-org-store-link)
  (define-key my-org-link-prefix-map (kbd "C-n") 'org-next-link)
  (define-key my-org-link-prefix-map (kbd "C-p") 'org-previous-link)
  (define-key my-org-link-prefix-map (kbd "C-r") 'my-org-refile-link)
  (define-key my-org-link-prefix-map (kbd "C-k") 'my-org-kill-link)
  (define-key my-org-link-prefix-map (kbd "C-f") 'my-org-link-file))

;;; link bindings
(define-key global-map (kbd "C-c l") 'org-store-link)
(with-eval-after-load 'org
  ;; first make sure C-c C-l is not bound so that it can serve as a prefix
  (define-key org-mode-map (kbd "C-c C-l") nil)
  (define-key org-mode-map (kbd "C-c C-l") my-org-link-prefix-map))

;;════════════════════════════════════════════════════════════
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
          (goto-char start) (insert "=")
          (goto-char (1+ end)) (insert "="))
      (insert "==") (backward-char))))

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

;;════════════════════════════════════════════════════════════

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

;;════════════════════════════════════════════════════════════

(defun my-org-meta-return-advice (FUN &optional arg)
  (if (equal arg '(64))
      (my-org-insert-random-child)
    (funcall FUN arg)))

(advice-add 'org-meta-return :around 'my-org-meta-return-advice)

;;════════════════════════════════════════════════════════════

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
                         (let ((subtree (buffer-substring-no-properties
                                         (point) (progn (org-end-of-subtree t t) (point)))))
                           (with-current-buffer temp-buffer
                             (save-excursion (insert subtree))
                             (my-org-tree-set-level 1)
                             (end-of-buffer)))
                         ;; only continue the loop when
                         ;; on a heading after the subtree
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

;;════════════════════════════════════════════════════════════
;; opening ID links should visit them in the same buffer

(with-eval-after-load 'org
  (setcdr (assoc 'file org-link-frame-setup)
          'find-file))

;;════════════════════════════════════════════════════════════
;; my-org-ring

(defvar my-org-ring (circlist-make nil)
  "A ring of org-mode entry positions which I want to use for myself.
Many commands use the org-mark-ring, but I want a ring that I am in total control of
and whose positions are always explictily set.")

(defun my-org-ring--check nil
  (when (circlist-empty-p my-org-ring)
    (user-error "Ring is empty")))

(defun my-org-ring-empty nil
  (interactive)
  (while (not (circlist-empty-p my-org-ring))
    (my-org-ring-remove)))

(defun my-org-ring-push (&optional before)
  (interactive "P")
  (save-excursion
    (org-back-to-heading t)
    (let ((marker (point-marker)))
      (put-text-property (point) (1+ (point)) :my-org-ring-marker marker)
      (circlist-add my-org-ring marker (if before :before :after)))))

(defun my-org-ring-jump nil
  (interactive)
  (let (jump-p marker)
    (while (and (not (circlist-empty-p my-org-ring)) (not jump-p))
      (setq marker (circlist-current my-org-ring))
      (ignore-errors
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            (when (invisible-p (point))
              (org-show-set-visibility 'canonical))
            (when (eq marker (get-text-property (point) :my-org-ring-marker))
              (setq jump-p t)))))
      (unless jump-p (circlist-pop my-org-ring)))
    (if (not (circlist-empty-p my-org-ring))
        (progn
          (my-jump-to-marker marker)
          (when (org-invisible-p) (org-show-set-visibility 'canonical)))
      (my-org-ring--check))))

(defun my-org-ring-next nil
  (interactive)
  (my-org-ring--check)
  (circlist-rotate my-org-ring :next)
  (my-org-ring-jump))

(defun my-org-ring-prev nil
  (interactive)
  (my-org-ring--check)
  (circlist-rotate my-org-ring :next)
  (my-org-ring-jump))

(defun my-org-ring-remove (&optional jump)
  (interactive "P")
  (my-org-ring--check)
  (my-org-ring--remove)
  (when jump (my-org-ring-jump)))

(defun my-org-ring--remove nil
  (let ((marker (circlist-pop my-org-ring)))
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
  (circlist-rotate my-org-ring :next))

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

;;════════════════════════════════════════════════════════════

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

;;════════════════════════════════════════════════════════════
;; * clones

(defvar my-org-id-link-re
  (let* ((id-re "id:\\(.*?\\)")
         (link-re (format "%s\\|\\[\\[%s\\]\\(?:\\[.*?\\]\\)\\]" id-re id-re)))
    link-re)
  "A regexp which matches an ID link to an entry. The actual ID is in group 1 or 2.
 To get it run (or (match-string 1) (match-string 2))")

(defun my-org-clone-fetch (eid)
  "Insert as a sibling to the current tree the tree whose ID is EID"
  (let ((tree (car (my-org-id-get eid '(tree)))))
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
        (org-with-point-at (cdr orig-location)
          (org-back-to-heading t)
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

;;════════════════════════════════════════════════════════════
;; * misc

(defsubst my-org-move-to-title nil
  "Move to the beginning of the title of the current heading, or just to the
beginning of the heading when it has no title."
  (org-back-to-heading t)
  (looking-at org-complex-heading-regexp)
  (when (match-string 4) (goto-char (match-beginning 4))))

(defsubst org-goto-root nil
  (while (org-up-heading-safe)))

(defun my-org-level-count (level)
  (interactive "nLevel: ")
  (let ((count 0))
    (org-map-region
     (lambda nil (when (= level (org-current-level)) (cl-incf count)))
     (point-min) (point-max))
    (if (called-interactively-p t) (message "%s" count) count)))

(defsubst org-goto-first-heading nil
  "Move to the first heading in the buffer"
  (interactive)
  (beginning-of-buffer)
  (unless (org-on-heading-p) (outline-next-heading)))

(defsubst org-insert-child nil
  (org-insert-heading-after-current) (org-demote))

(defun my-org-id-get (eid things)
  "Read data about the tree whose global ID is EID.
THINGS is a list of symbols which specifies what to get. The things are returned
in the order in which they are specified. Look at the COND to see the choices
available."
  (let ((marker (org-id-find eid :marker)))
    (when marker
      (org-with-point-at marker
        (mapcar
         (lambda (thing)
           (cond ((eq thing 'position) (point))
                 ((eq thing 'tree) (my-org-tree-text :no-properties))
                 ((eq thing 'title) (org-no-properties (org-get-heading t t t t)))
                 ((eq thing 'buffer) (current-buffer))
                 ((eq thing 'file) (buffer-file-name (buffer-base-buffer)))
                 ((eq thing 'marker) marker)
                 ((eq thing 'heading) (org-get-heading))
                 (t (error "Invalid THING: %S" thing))))
         things)))))

(defun my-org-id-remove nil
  (let ((id (org-entry-get nil "ID")))
    (when id
      (org-entry-delete nil "ID")
      (remhash id org-id-locations))))

(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done nil)

(setq-default org-startup-folded t)
(add-hook 'org-mode-hook (lambda nil (electric-indent-local-mode -1)))

(defun my-org-isearch-headlines nil
  (interactive)
  (let ((isearch-filter-predicate
         (lambda (start end)
           (save-match-data
             (isearch-filter-visible start end)
             (save-excursion (goto-char start) (org-on-heading-p))))))
    (isearch-mode t nil nil t)))

(define-key org-mode-map (kbd "M-s e") 'my-org-isearch-headlines)

;; without this, I get a "Running less..." error when
;; trying to open file links within Emacs
(defun my-org-open-at-point (&optional arg)
  (interactive "P")
  (if (not arg) (org-open-at-point '(4))
    (org-open-at-point nil)))
(define-key org-mode-map (kbd "C-c C-o") 'my-org-open-at-point)

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

(defun my-org-date-merge (paths buffer)
  (let ((date-map (make-hash-table :test 'equal))
        (date-heading-re (concat "^\\* *\\(" (org-re-timestamp 'inactive) "\\)"))
        date children filename filemap)
    ;; construct date-map
    (dolist (path paths)
      (setq filename (file-name-nondirectory path))
      (with-current-buffer (find-file-noselect path)
        (org-with-point-at 1
          (while (re-search-forward date-heading-re nil t)
            (setq date (match-string 1)
                  children (buffer-substring (line-beginning-position 2)
                                             (my-org-tree-end-pos t t)))
            (puthash date
                     (cons (cons filename children)
                           (gethash date date-map))
                     date-map)))))
    (with-current-buffer (get-buffer-create buffer)
      (dolist (date (sort (hash-table-keys date-map) 'string<))
        (insert (format "* %s\n" date))
        (dolist (file+children (gethash date date-map))
          (let ((file (car file+children))
                (children (cdr file+children)))
            (insert (format "** %s\n" file))
            (save-excursion (insert children))
            (org-map-region 'org-demote (point) (progn (end-of-buffer) (point)))))))))

(defun my-org-strip-links (string)
  (with-temp-buffer
    (insert string) (beginning-of-buffer)
    (while (re-search-forward org-link-any-re nil t)
      (replace-match (match-string 3)))
    (buffer-string)))

;; Fixed the bug of "READ" showing "READ_L" as well
(defun org-show-todo-tree (arg)
  "Make a compact tree which shows all headlines marked with TODO.
The tree will show the lines where the regexp matches, and all higher
headlines above the match.
With a `\\[universal-argument]' prefix, prompt for a regexp to match.
With a numeric prefix N, construct a sparse tree for the Nth element
of `org-todo-keywords-1'."
  (interactive "P")
  (let ((case-fold-search nil)
	(kwd-re
	 (cond ((null arg) (concat org-not-done-regexp "\\s-"))
	       ((equal arg '(4))
		(let ((kwd
		       (completing-read "Keyword (or KWD1|KWD2|...): "
					(mapcar #'list org-todo-keywords-1))))
		  (concat "\\("
			  (mapconcat 'identity (org-split-string kwd "|") "\\|")
			  "\\)\\>" "\\s-")))
	       ((<= (prefix-numeric-value arg) (length org-todo-keywords-1))
		(regexp-quote (nth (1- (prefix-numeric-value arg))
				   org-todo-keywords-1)))
	       (t (user-error "Invalid prefix argument: %s" arg)))))
    (message "%d TODO entries found"
	     (org-occur (concat "^" org-outline-regexp " *" kwd-re )))))

(setq org-highlight-sparse-tree-matches nil)

;;════════════════════════════════════════
;; misc_yanking_images

(defvar my-org-images-dir "~/leng/images/")

(defun my-org-yank-image--filename nil
  (let ((basename
         (concat "paste-" (number-to-string (time-convert nil 'integer))
                 ".png")))
    (concat my-org-images-dir basename)))

(defun my-org-yank-image--shell-command (filename)
  (format "xclip -selection clipboard -t image/png -o > %s"
          filename))

(defun my-org-yank-image nil
  (interactive)
  (when (not (bolp)) (insert "\n"))
  (insert "\n") (backward-char)
  (let* ((file (my-org-yank-image--filename))
         (command (my-org-yank-image--shell-command file))
         (link (format "[[%s]]" file))
         start end)
    (shell-command command)
    (setq start (point)) (insert link) (setq end (point))
    (org-display-inline-images nil nil start end)))

(setq org-startup-with-inline-images t)

;;════════════════════
;; misc-org-select

(defvar org-select--buffer-name "*org-select*")
;; TODO: Use `defface'
(defvar org-select-current-face '(:foreground "white" :background "black")
  "The face of the heading corresponding to the current node")

(define-derived-mode org-select-mode org-mode "Select-Node"
  (setq cursor-type nil)
  (read-only-mode)
  (make-local-variable 'post-command-hook)
  (add-hook
   'post-command-hook
   (lambda nil
     (if (org-before-first-heading-p)
         (org-next-visible-heading)
       (org-back-to-heading))
     (org-select-highlight-current))))

(defvar-local org-select-did-quit nil)
(defvar-local org-select-list-items nil)
(defvar-local org-select-list-prelude nil)

(defun org-select-list (items &optional prelude default)
  "Ask the user to select an element of ITEMS.  ITEMS must be an alist. The keys
are the headings to display and the values are arbitrary data. The function
returns the item selected by the user, or nil when the user quits without
selecting anything. PRELUDE is informative text to display before the
items. DEFAULT must be an element of ITEMS, and supplying it has the effect of
highlighting that item when the window is first shown"
  (when items
    (let ((selection-buffer (get-buffer-create org-select--buffer-name))
          result)
      (switch-to-buffer-other-window selection-buffer)
      (org-select-mode)
      (setq org-select-did-quit nil
            org-select-list-items items
            org-select-list-prelude prelude)
      (org-select-list-redraw)
      (when default
        (text-property-search-forward :item default 'eq))
      (recursive-edit)
      (setq result (and (not org-select-did-quit) (get-text-property (point) :item)))
      (kill-buffer-and-window)
      result)))

(defun org-select-list-redraw nil
  (let ((inhibit-read-only t))
    (erase-buffer)
    (when org-select-list-prelude
      (insert org-select-list-prelude "\n"))
    (dolist (heading-and-data org-select-list-items)
      (save-excursion (insert "* " (car heading-and-data) "\n"))
      (put-text-property (point) (1+ (point)) :item heading-and-data)
      (forward-line))
    (org-goto-first-heading)))

(defun org-select-highlight-current nil
  "Assumes it is called in the org-select buffer. Highlights the heading at point"
  (my-unhighlight-all)
  (when (invisible-p (point)) (org-show-set-visibility 'canonical))
  (let ((my-highlight-face org-select-current-face))
    (my-highlight-region (line-beginning-position) (line-end-position))))

(defun org-select-exit nil
  (interactive)
  (exit-recursive-edit))

(defun org-select-quit nil
  (interactive)
  (setq org-select-did-quit t)
  (exit-recursive-edit))

(defvar org-select-mode-map (make-sparse-keymap))
(progn
  (define-key org-select-mode-map "p" 'org-previous-visible-heading)
  (define-key org-select-mode-map "n" 'org-next-visible-heading)
  (define-key org-select-mode-map "f" 'org-forward-heading-same-level)
  (define-key org-select-mode-map "b" 'org-backward-heading-same-level)
  (define-key org-select-mode-map "u" 'outline-up-heading)
  (define-key org-select-mode-map "q" 'org-select-quit)
  (define-key org-select-mode-map (kbd "RET") 'org-select-exit))

;;════════════════════
;; misc-trees

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

(defsubst my-org-tree-delete (&optional extract)
  (save-excursion
    (org-back-to-heading t)
    (funcall (if extract 'delete-and-extract-region 'delete-region)
             (point) (my-org-tree-end-pos t t))))

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

;;════════════════════════════════════════
;; TEMP and TEMPDONE

(defun my-org-tempdone-after-state-change nil
  (if (string= org-state "TEMPDONE")
      (unless (string= org-last-state "TEMPDONE")
        (org-entry-put (point) "TEMPDONE_UNDO" org-last-state)
        (org-flag-subtree t))
    (when (string= org-last-state "TEMPDONE")
      (org-entry-delete nil "TEMPDONE_UNDO")
      (org-entry-delete nil "TEMPDONE_UNDO_DAY"))))

(defun my-org-tempdone (&optional interval)
  (let ((state (org-get-todo-state))
        (org-after-todo-state-change-hook
         (remq 'my-org-tempdone-after-state-change
               org-after-todo-state-change-hook))
        (day (and interval (+ (my-time-today) interval))))
    (unless (string= state "TEMPDONE")
      (org-todo "TEMPDONE")
      (org-entry-put nil "TEMPDONE_UNDO" state)
      (org-entry-put nil "TEMPDONE_INTERVAL" (number-to-string interval))
      (when day
        (org-entry-put nil "TEMPDONE_UNDO_DAY" (number-to-string day)))      
      (org-back-to-heading) (org-flag-subtree nil))))

(defvar my-org-tempdone-random-interval (cons 8 12))
(defun my-org-tempdone-random nil
  (interactive)
  (my-org-tempdone
   (my-randint
    (car my-org-tempdone-random-interval)
    (cdr my-org-tempdone-random-interval))))

(defun my-org-tempdone-interval (&optional arg)
  (interactive "P")
  (let* (interval prompt interval)
    (if arg
        (progn (setq interval (read-number "Interval: "))
               (org-map-tree
                (lambda nil
                  (when (org-entry-is-todo-p) (my-org-tempdone interval)))))
      (setq interval (org-entry-get nil "TEMPDONE_INTERVAL"))
      (if interval
          (setq interval (string-to-number interval)
                prompt (format "Interval (last was %s): " interval))
        (setq prompt "Interval: "))
      (setq interval (read-number prompt))
      (my-org-tempdone interval))))

(defun my-org-tempdone-undo-buffer nil
  "Go through the accessible portion of the current buffer and undo the TEMDONE entries"
  (interactive)
  (my-org-tempdone-undo-region (point-min) (point-max)))

(defun my-org-tempdone-undo-region (start end &optional force)
  (interactive "r")
  (org-map-region (apply-partially 'my-org-tempdone-undo force) start end)
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

(defun my-org-tempdone-sched-READ (&optional interval)
  (interactive "nInterval: ")
  (org-todo "READ") (my-org-tempdone interval))

(add-hook 'org-after-todo-state-change-hook
          'my-org-tempdone-after-state-change)
(add-hook 'org-mode-hook
          'my-org-tempdone-undo-buffer)

;;════════════════════════════════════════════════════════════
;; nodes

(defun my-org-node-date nil
  (interactive)
  (let ((timestamp (with-temp-buffer
                     (org-insert-time-stamp (current-time) nil :inactive)
                     (buffer-substring-no-properties (point-min) (point-max)))))
    (org-entry-put nil "DATE" timestamp)))

(defun my-org-node-add-source nil
  (interactive)
  (let ((source (read-string "SOURCE: ")))
    (org-entry-put nil "SOURCE" source)))

(defun my-org-node-bury nil
  "Move the node at point to become the last child of its parent"
  (interactive)
  (org-back-to-heading)
  (save-excursion
    (let ((point (point))
          (root-p (= 1 (org-current-level)))
          (parent-pos (save-excursion (org-up-heading-safe) (point)))
          (text (my-org-tree-delete :extract)))
      (if root-p
          (progn (end-of-buffer) (ensure-newline))
        (goto-char parent-pos) (org-end-of-subtree t t) (ensure-newline))
      (save-excursion (insert text)) (org-show-entry) (org-flag-subtree t))))

(defun my-org-node-show nil
  (interactive)
  (org-show-set-visibility 'canonical))

(defvar my-org-node-put-props
  '("GENERAL" "CONTRIBUTION" "DISCUSS"))
(defun my-org-node-put (property)
  (interactive (list (completing-read "PROPERTY: " my-org-node-put-props)))
  (org-entry-put nil property "t"))

;; navigation
;; ════════════════════
(defvar org-nav-expr nil)

(defun org-nav-next (&optional arg)
  (interactive "P")
  (org--nav-move arg :next))

(defun org-nav-prev (&optional arg)
  (interactive "P")
  (org--nav-move arg :prev))

(defun org--nav-move (recompute-p direction)
  (when recompute-p
    (setq org-nav-expr (read--expression "Predicate: ")))
  (unless org-nav-expr
    (user-error "org-nav-expr is nil"))
  (unless overriding-terminal-local-map
    (set-transient-map org-nav-transient-map t))
  (let ((move-func (if (eq direction :next) 'outline-next-heading
                     'outline-previous-heading))
        (pred `(lambda nil ,org-nav-expr))
        (pos nil))
    (save-excursion
      (catch :break
        (while (funcall move-func)
          (when (funcall pred)
            (setq pos (point)) (throw :break nil)))))
    (if pos
      (progn (goto-char pos)
             (when (org-invisible-p)
               (org-show-set-visibility 'canonical)))
      (user-error "No matching nodes"))))

(defvar org-nav-transient-map (make-sparse-keymap))
(progn
  (define-key org-nav-transient-map "n" 'org-nav-next)
  (define-key org-nav-transient-map "p" 'org-nav-prev))

;; keymap
;; ════════════════════
(defvar my-org-node-map (make-sparse-keymap)
  "Binds keys to commands which work on nodes")
(progn
  (define-key my-org-node-map "t" 'my-org-tempdone-interval)
  (define-key my-org-node-map "r" 'my-org-tempdone-sched-READ)
  (define-key my-org-node-map "}" 'my-org-tempdone-random)
  (define-key my-org-node-map "d" 'my-org-node-date)
  (define-key my-org-node-map "s" 'my-org-node-add-source)
  (define-key my-org-node-map "b" 'my-org-node-bury)
  (define-key my-org-node-map "u" 'my-org-node-put)
  (define-key my-org-node-map "h" 'my-org-node-show)
  (define-key my-org-node-map "n" 'org-nav-next)
  (define-key my-org-node-map "p" 'org-nav-prev))
(define-key org-mode-map "\C-ce" my-org-node-map)
(define-key org-mode-map (kbd "C-.") 'my-org-node-bury)

;;════════════════════════════════════════════════════════════
;; org-dyn

(defvar-local org-dyn-buffer-A nil)
(defun org-dyn-create (buffer-B pred)
  "In BUFFER-B create a dynamic view based on which entries pass PRED in the
current buffer. PRED is a predicate function called at the beginning of each
entry, except that if an entry passes PRED the search continues past its tree"
  ;; setup BUFFER-B
  (setq buffer-B (get-buffer-create buffer-B))
  (let ((buffer-A (current-buffer)))
    (with-current-buffer buffer-B
      (erase-buffer) (org-mode)
      (setq org-dyn-buffer-A buffer-A)))
  (save-excursion
    (beginning-of-buffer)
    (when (or (org-on-heading-p) (outline-next-heading))
      (do-while
        (if (save-excursion (funcall pred))
            (let* ((connection (org-dyn--connection-get-create))
                   (tree-beginning (point)) (tree-end (my-org-tree-end-pos t t))
                   (subtree (buffer-substring-no-properties
                             tree-beginning tree-end)))
              (org-dyn--connect connection) (goto-char tree-end)
              (with-current-buffer buffer-B
                (save-excursion (insert subtree))
                (my-org-tree-set-level 1) (org-dyn--connect connection)
                (end-of-buffer))
              ;; only continue the loop when
              ;; on a heading after the subtree
              (not (eobp)))
          (outline-next-heading)))))
  (with-current-buffer buffer-B
    (org-set-startup-visibility)))

(defun org-dyn--check-dyn-buffer nil
  (unless org-dyn-buffer-A
    (user-error "Current buffer not a dynamic buffer"))
  (unless (buffer-live-p org-dyn-buffer-A)
    (user-error "BUFFER-A is not alive")))

(defun org-dyn-get-connection nil
  "Move to the root of the dynamic tree and
return the value of its connection property"
  (let ((connection (org-dyn--up)))
    (unless connection
      (user-error "missing connecting text property"))
    connection))

(defsubst org-dyn--get-connection nil
  "Assumes it is called with point at the beginning of a heading"
  (let ((connection (get-text-property (point) :org-dyn-connection))
        (pos nil))
    (or connection
        (progn
          (setq pos (next-single-property-change
                     (point) :org-dyn-connection nil (line-end-position)))
          (and pos (get-text-property pos :org-dyn-connection))))))

(defvar org-dyn--next-connection 0)
(defun org-dyn--connection-get-create nil
  (or (org-dyn--get-connection)
      (prog1 org-dyn--next-connection
        (setq org-dyn--next-connection (1+ org-dyn--next-connection)))))

(defun org-dyn--find (connection)
  (beginning-of-buffer)
  (if (text-property-search-forward :org-dyn-connection connection 'eq)
      (beginning-of-line)
    (user-error "Cannot find NODE-A")))

(defun org-dyn--connect (connection)
  "Assumes it is called on a heading"
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (put-text-property start end :org-dyn-connection connection)
    (put-text-property start end 'rear-nonsticky (list :org-dyn-connection))))

(defun org-dyn--up nil
  "Move up the ancestors until you arrive at a node with a connection property
and return the value of the property."
  (let (connection)
    (org-back-to-heading)
    (loop
      (if (setq connection (org-dyn--get-connection))
          (end-loop)
        (unless (org-up-heading-safe) (end-loop))))
    connection))
  
(defun org-dyn-goto-original nil
  (interactive)
  (let (connection)
    (org-dyn--check-dyn-buffer)
    (save-excursion
      (setq connection (org-dyn-get-connection))
      (switch-to-buffer org-dyn-buffer-A)
      (org-dyn--find connection)
      (when (invisible-p (point))
        (org-show-set-visibility 'canonical)))))

(defun org-dyn-push nil
  (interactive)
  (org-dyn--check-dyn-buffer)
  (save-excursion
    (let* ((connection (org-dyn-get-connection))
           (title (org-no-properties (org-get-heading t t t t)))
          level-A text-B visibility-B)
      (setq text-B (my-org-tree-text :no-properties)
            visibility-B (my-org-tree-get-visibility))
      (with-current-buffer org-dyn-buffer-A
        (org-with-wide-buffer
         (org-dyn--find connection)
         (setq level-A (funcall outline-level))
         (narrow-to-region (point) (my-org-tree-end-pos t t))
         (delete-region (point-min) (point-max))
         (save-excursion (insert text-B))
         (my-org-tree-set-visibility visibility-B)
         (my-org-tree-set-level level-A)
         (org-dyn--connect connection)))
      (message "Pushed \"%s\"" title))))

(defun org-dyn-from-expr nil
  (interactive)
  (let* ((buffer-name (read-buffer "Buffer: "))
         (pred-body (read--expression "Boolean expr: "))
         (pred `(lambda nil ,pred-body)))
    (org-dyn-create buffer-name pred)
    (switch-to-buffer buffer-name)))

(defun org-dyn-remove-AB nil
  "Removes the entry at point and its connected entry"
  (interactive)
  (org-dyn--check-dyn-buffer)
  (let* (root-pos connection)
    (setq root-pos
          (save-excursion
            (setq connection (org-dyn-get-connection)) (point)))
    (org-dyn--remove-A connection)
    (goto-char root-pos) (my-org-tree-delete)))

(defun org-dyn--remove-A (connection)
  "Signals an error when the node-A cannot be found in `org-dyn-buffer-A'"
  (with-current-buffer org-dyn-buffer-A
    (org-dyn--find connection)
    (my-org-tree-delete)))

(defun org-dyn-remove-A-region (start end)
  "Remove from buffer-A all headings which correspond to a dynamic heading in
the region in the current dynamic buffer"
  (interactive "r")
  (org-dyn--check-dyn-buffer)
  (let (connection failures)
    (org-map-region
     (lambda nil
       (when (setq connection (org-dyn--get-connection))
         (condition-case nil
             (org-dyn--remove-A connection)
           (error (push (org-get-heading t t t t) failures)))))
     start end)
    (if failures
        (with-help-window "*org-dyn-log*"
          (princ "The following nodes could not be deleted: \n")
          (princ (mapconcat (lambda (title) (concat "* " title))
                            failures "\n")))
      (message "All removed successfully"))))

(defvar org-dyn-map (make-sparse-keymap))
(progn
  (define-key org-dyn-map "\C-g" 'org-dyn-goto-original)
  (define-key org-dyn-map "\C-p" 'org-dyn-push)
  (define-key org-dyn-map "\C-e" 'org-dyn-from-expr)
  (define-key org-dyn-map "\C-r" 'org-dyn-remove-AB))
(define-key org-mode-map (kbd "C-c C-d") org-dyn-map)

(defun org-dyn-cleanup nil
  "A hook for `kill-buffer-hook'. Removes the connections from the original buffer"
  (when (buffer-live-p org-dyn-buffer-A)
    (with-current-buffer org-dyn-buffer-A
      (org-with-wide-buffer
       (remove-text-properties
        (point-min) (point-max)
        (list :org-dyn-connection nil))))))
(add-hook 'kill-buffer-hook 'org-dyn-cleanup)

;;════════════════════════════════════════
;; org-pplist
;;════════════════════
;; org-pplist-utils

(defvar org-pplist--id-property (make-symbol "org-pplist-id")
  "The text property which associates a line in a pplist buffer with an entry
ID. The ID is the same one you would get if you were to parse the line and
fetch the :id attribute")

(defsubst org-pplist--associate-line (eid)
  "Assumes it is called inside a pplist buffer and that point is on the
beginning of the line.  Associates the current line in with EID."
  (put-text-property (point) (1+ (point)) org-pplist--id-property eid))

(defun org-pplist--flush (pplist)
  (with-current-buffer (plist-get pplist :buffer)
    (write-region nil nil (buffer-file-name) nil :no-write-message)))

(defun org-pplist--goto-plist (eid)
  "Assumes it is called inside the buffer of a pplist. Moves point to the
beginning of the line representing the plist corresponding to EID and returns
t. If no such line exists, returns nil"
  (beginning-of-buffer)
  (when (text-property-search-forward org-pplist--id-property eid 'equal)
    (beginning-of-line) :found))
  
;;════════════════════
;; org-pplist-operations

(defun org-pplist-make (path)
  "Make a persistent plist corresponding to the file at PATH"
  (let (buffer list result)
    (if (setq buffer (get-file-buffer path))
        (buffer-local-value 'org-pplist buffer)
      (setq buffer (find-file-noselect path))
      (with-current-buffer buffer
        (emacs-lisp-mode)
        (rename-buffer (concat " *" (file-name-nondirectory path) "*"))
        (setq list (my-read-buffer))
        ;; associate lines with IDs
        (beginning-of-buffer)
        (dolist (plist list)
          (org-pplist--associate-line (plist-get plist :id))
          (beginning-of-line 2))
        ;; avoid query when killing
        (set-buffer-modified-p nil)
        (setq result (list :buffer buffer :path path :list list))
        (setq-local org-pplist result)
        result))))

(defun org-pplist-get (pplist eid)
  "Return the plist corresponding to EID in PPLIST"
  (when eid
    (seq-find (lambda (plist) (equal eid (plist-get plist :id)))
              (plist-get pplist :list))))

(defun org-pplist-add (pplist eid plist &optional flush)
  (when (org-pplist-get pplist eid)
    (error "plist corresponding to \"%s\" already exists" eid))
  (setq plist (append (list :id eid) plist))
  (plist-put pplist :list
    (nconc (plist-get pplist :list) (list plist)))
  ;; insert in pplist buffer
  (with-current-buffer (plist-get pplist :buffer)
    (end-of-buffer)
    (prin1 plist (current-buffer)) (insert "\n")
    (beginning-of-line 0)
    (org-pplist--associate-line eid)
    (set-buffer-modified-p nil)
    (when flush (org-pplist--flush pplist)))
  plist)

(defun org-pplist-updated-plist (pplist plist &optional flush)
  "This function must be called each time a plist of PPLIST is modified.
Supplying a non-nil FLUSH argument will result in flushing PPLIST. Assumes that
PLIST belongs to PPLIST."
  (with-current-buffer (plist-get pplist :buffer)
    (unless (org-pplist--goto-plist (plist-get plist :id))
      (error "plist for \"%s\"not in pplist buffer" (plist-get plist :id)))
    (delete-region (point) (line-end-position))
    (prin1 plist (current-buffer))
    (beginning-of-line)
    (org-pplist--associate-line (plist-get plist :id))
    (set-buffer-modified-p nil)
    (when flush (org-pplist--flush pplist))))

(defun org-pplist-remove (pplist eid &optional flush)
  "Delete EID's plist from PPLIST and return t. If EID is has no plist in PPLIST, return nil"
  (let ((list-and-flag (LIST-delete-and-tell
                        (plist-get pplist :list)
                        (lambda (elt) (string= (plist-get elt :id) eid)))))
    (when (cdr list-and-flag)
      (plist-put pplist :list (car list-and-flag))
      (with-current-buffer (plist-get pplist :buffer)
        (unless (org-pplist--goto-plist eid)
          (error "Cannot find %S in pplist buffer" eid))
        (delete-current-line)
        (set-buffer-modified-p nil)
        (when flush (org-pplist--flush pplist))
        t))))

;;════════════════════════════════════════════════════════════
;; org-state

(defvar org-state-states (circlist-make nil))

(defun org-state-clear nil
  (interactive)
  (setq org-state-states (circlist-make nil))
  (message "org-state: cleared"))

(defun org-state-save nil
  (interactive)
  (let ((string (buffer-string)))
    (circlist-add org-state-states string :next)
    (circlist-rotate org-state-states :next))
  (message "org-state: saved"))

(defun org-state-store (path)
  "Stores in the kill ring a serialization of the current state sequence"
  (interactive "FFile: ")
  (with-temp-file path
    (pp (mapcar 'org-no-properties (circlist-to-list org-state-states))
        (current-buffer))))

(defun org-state-load (path)
  (interactive "fFile: ")
  (with-temp-buffer
    (insert-file-contents path)
    (beginning-of-buffer)
    (let ((list (read (current-buffer))))
      (setq org-state-states (circlist-make list)))))

(defun org-state-pop nil
  (interactive)
  (circlist-pop org-state-states :prev)
  (message "org-state: popped"))

(defun org-state-next nil
  (interactive)
  (circlist-rotate org-state-states :next)
  (org-state-current))

(defun org-state-prev nil
  (interactive)
  (circlist-rotate org-state-states :prev)
  (org-state-current))

(defun org-state-current nil
  (interactive)
  (delete-region (point-min) (point-max))
  (save-excursion (insert (circlist-current org-state-states))))

(defvar org-state-transient-map (make-sparse-keymap))
(progn
  (define-key org-state-transient-map "n" 'org-state-next)
  (define-key org-state-transient-map "p" 'org-state-prev))
(defun org-state-navigate nil
  (interactive)
  (set-transient-map org-state-transient-map t))

(defvar org-state-map (make-sparse-keymap))
(progn
  (define-key org-state-map "p" 'org-state-prev)
  (define-key org-state-map "n" 'org-state-next)
  (define-key org-state-map "c" 'org-state-current)
  (define-key org-state-map "v" 'org-state-navigate)
  (define-key org-state-map "s" 'org-state-save)
  (define-key org-state-map "o" 'org-state-pop)
  (define-key org-state-map " " 'org-state-clear))
(define-key org-mode-map "\C-cp" org-state-map)

;; function fixes
;; ════════════════════════════════════════

;; so that newlines after an item aren't indented
(defun org--get-expected-indentation (element contentsp)
  "Expected indentation column for current line, according to ELEMENT.
ELEMENT is an element containing point.  CONTENTSP is non-nil
when indentation is to be computed according to contents of
ELEMENT."
  (let ((type (org-element-type element))
	(start (org-element-property :begin element))
	(post-affiliated (org-element-property :post-affiliated element)))
    (org-with-wide-buffer
     (cond
      (contentsp
       (cl-case type
	 ((diary-sexp footnote-definition) 0)
	 ((headline inlinetask nil)
	  (if (not org-adapt-indentation) 0
	    (let ((level (org-current-level)))
	      (if level (1+ level) 0))))
	 (t
	  (goto-char start)
	  (current-indentation))))
      ((and
	(eq org-adapt-indentation 'headline-data)
	(memq type '(planning clock node-property property-drawer drawer)))
       (org--get-expected-indentation
	(org-element-property :parent element) t))
      ((memq type '(headline inlinetask nil))
       (if (org-match-line "[ \t]*$")
	   (org--get-expected-indentation element t)
	 0))
      ((memq type '(diary-sexp footnote-definition)) 0)
      ;; First paragraph of a footnote definition or an item.
      ;; Indent like parent.
      ((< (line-beginning-position) start)
       (org--get-expected-indentation
	(org-element-property :parent element) t))
      ;; At first line: indent according to previous sibling, if any,
      ;; ignoring footnote definitions and inline tasks, or parent's
      ;; contents.
      ((and ( = (line-beginning-position) start)
	    (eq org-adapt-indentation t))
       (catch 'exit
	 (while t
	   (if (= (point-min) start) (throw 'exit 0)
	     (goto-char (1- start))
	     (let* ((previous (org-element-at-point))
		    (parent previous))
	       (while (and parent (<= (org-element-property :end parent) start))
		 (setq previous parent
		       parent (org-element-property :parent parent)))
	       (cond
		((not previous) (throw 'exit 0))
		((> (org-element-property :end previous) start)
		 (throw 'exit (org--get-expected-indentation previous t)))
		((memq (org-element-type previous)
		       '(footnote-definition inlinetask))
		 (setq start (org-element-property :begin previous)))
		(t (goto-char (org-element-property :begin previous))
		   (throw 'exit
			  (if (bolp) (current-indentation)
			    ;; At first paragraph in an item or
			    ;; a footnote definition.
			    (org--get-expected-indentation
			     (org-element-property :parent previous) t))))))))))
      ;; Otherwise, move to the first non-blank line above.
      ((not (eq org-adapt-indentation 'headline-data))
       (beginning-of-line)
       (let ((pos (point)))
	 (skip-chars-backward " \r\t\n")
	 (cond
	  ;; Two blank lines end a footnote definition or a plain
	  ;; list.  When we indent an empty line after them, the
	  ;; containing list or footnote definition is over, so it
	  ;; qualifies as a previous sibling.  Therefore, we indent
	  ;; like its first line.
	  ((and (memq type '(footnote-definition plain-list))
		(> (count-lines (point) pos) 2))
	   (goto-char start)
	   (current-indentation))
	  ;; Line above is the first one of a paragraph at the
	  ;; beginning of an item or a footnote definition.  Indent
	  ;; like parent.
	  ((< (line-beginning-position) start)
	   (org--get-expected-indentation
	    (org-element-property :parent element) t))
	  ;; Line above is the beginning of an element, i.e., point
	  ;; was originally on the blank lines between element's start
	  ;; and contents.
	  ((= (line-beginning-position) post-affiliated)
	   (org--get-expected-indentation element t))
	  ;; POS is after contents in a greater element.  Indent like
	  ;; the beginning of the element.
	  ((and (memq type org-element-greater-elements)
		(let ((cend (org-element-property :contents-end element)))
		  (and cend (<= cend pos))))
	   ;; As a special case, if point is at the end of a footnote
	   ;; definition or an item, indent like the very last element
	   ;; within.  If that last element is an item, indent like
	   ;; its contents.
	   (if (memq type '(footnote-definition item plain-list))
	       (let ((last (org-element-at-point)))
		 (goto-char pos)
		 (org--get-expected-indentation
		  last (eq (org-element-type last) 'item)))
	     (goto-char start)
	     (current-indentation)))
	  ;; In any other case, indent like the current line.
	  (t (current-indentation)))))
      ;; Finally, no indentation is needed, fall back to 0.
      (t (current-indentation))))))


;; ════════════════════════════════════════
(provide 'my-org)
