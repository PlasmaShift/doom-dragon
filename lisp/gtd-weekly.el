;;; gtd-weekly.el --- Saturday-start weekly GTD journals with Denote -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; One Org file per week in `my/gtd-directory', week starts Saturday.
;; Days: Sat–Fri with Core / Secondary / Unplanned, plus Staging.
;; Capture targets today-or-staging × Core/Secondary/Unplanned.
;; Soft focus limit: more than `my/gtd-core-limit' open Core tasks for today.

;;; Code:

(require 'org)
(require 'org-element)
(require 'denote)
(require 'cl-lib)

(defgroup my/gtd nil
  "Weekly GTD journals (Saturday week start)."
  :group 'denote)

(defcustom my/gtd-directory
  (expand-file-name "~/doc/denote/gtd")
  "Directory for weekly GTD journal files."
  :type 'directory
  :group 'my/gtd)

(defcustom my/gtd-keyword "weekly"
  "Denote keyword for weekly GTD files.

Suggestions for low keyword collision (pick one, change here and restart):
  - weekly     (current)
  - plan
  - focus
  - intent
  - wk
  - week-plan
  - gtd-plan
  - rhythm / cadence / sprint

The keyword appears in the file name as __<keyword>.org"
  :type 'string
  :group 'my/gtd)

(defcustom my/gtd-core-limit 3
  "Soft max open Core tasks for today.  Above this, warn to cut or demote."
  :type 'integer
  :group 'my/gtd)

(defcustom my/gtd-categories '("Core" "Secondary" "Unplanned")
  "Task category headlines under each day and Staging."
  :type '(repeat string)
  :group 'my/gtd)

(defvar my/gtd--capture-where 'today
  "Capture destination: `today' or `staging'.")

(defvar my/gtd--capture-category "Core"
  "Capture category: one of `my/gtd-categories'.")

;;;; Time helpers (week starts Saturday)

(defun my/gtd-week-start (&optional time)
  "Return the Saturday 00:00 that starts the week containing TIME.
TIME is Emacs time; default is now."
  (let* ((time (or time (current-time)))
         ;; %w: Sunday=0 … Saturday=6
         (dow (string-to-number (format-time-string "%w" time)))
         (days-since-sat (mod (- dow 6) 7))
         (sat (time-subtract time (days-to-time days-since-sat))))
    (encode-time 0 0 0
                 (string-to-number (format-time-string "%d" sat))
                 (string-to-number (format-time-string "%m" sat))
                 (string-to-number (format-time-string "%Y" sat)))))

(defun my/gtd-day-stamp (&optional time)
  "Org active timestamp date for TIME, e.g. <2026-07-26 Sun>."
  (format-time-string "<%Y-%m-%d %a>" (or time (current-time))))

(defun my/gtd-ensure-directory ()
  "Create `my/gtd-directory' if needed and return it."
  (let ((dir (file-name-as-directory (expand-file-name my/gtd-directory))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

;;;; Weekly file body

(defun my/gtd-week-body (&optional week-start)
  "Return Org body for the week starting at WEEK-START (Saturday)."
  (let* ((start (or week-start (my/gtd-week-start)))
         (days (cl-loop for i from 0 to 6
                        collect (time-add start (days-to-time i))))
         (day-blocks
          (mapconcat
           (lambda (day)
             (concat
              (format "* %s\n" (my/gtd-day-stamp day))
              (mapconcat (lambda (cat)
                           (format "** [/] %s\n" cat))
                         my/gtd-categories
                         "")))
           days
           ""))
         (staging
          (concat
           "* Staging\n"
           (mapconcat (lambda (cat)
                        (format "** [/] %s\n" cat))
                      my/gtd-categories
                      ""))))
    (concat
     "#+STARTUP: show2levels\n"
     "#+STARTUP: lognoteclock-out\n"
     "#+TODO: TODO | DONE\n"
     "\n"
     day-blocks
     staging)))

;;;; Find / create weekly file

(defun my/gtd--files-for-week (week-start)
  "Return Denote files in `my/gtd-directory' for WEEK-START Saturday."
  (let* ((dir (my/gtd-ensure-directory))
         (day (format-time-string "%Y%m%d" week-start))
         (denote-directory dir)
         (rx (format "\\`%sT[0-9]\\{6\\}.*%s"
                     (regexp-quote day)
                     (regexp-quote my/gtd-keyword))))
    (denote-directory-files rx)))

(defun my/gtd-weeks-in-year (&optional year)
  "Return number of ISO weeks in YEAR (52 or 53)."
  (let* ((y (or year (string-to-number (format-time-string "%Y"))))
         (dec28 (encode-time 0 0 0 28 12 y))
         (last-week (string-to-number (format-time-string "%V" dec28))))
    last-week))

(defun my/gtd-week-title (&optional week-start)
  "Return rich title with week number, weeks-in-year, month, start+end day.
Example: Week 31/52 — Jul 2026 (Sat 25 Jul – Fri 31 Jul)"
  (let* ((start (or week-start (my/gtd-week-start)))
         (end (time-add start (days-to-time 6)))
         (w (format-time-string "%V" start))
         (y (string-to-number (format-time-string "%Y" start)))
         (total-w (my/gtd-weeks-in-year y))
         (s (format-time-string "%a %d %b" start))
         (e (format-time-string "%a %d %b" end)))
    (format "Week %s/%d — %s %s (%s – %s)" w total-w y (format-time-string "%b" start) s e)))

(defun my/gtd-path-to-new-or-existing (&optional time)
  "Return path to this week's GTD file, *creating* it if missing.
Use for actual capture / open.  TIME selects which week."
  (let* ((week-start (my/gtd-week-start time))
         (dir (my/gtd-ensure-directory))
         (existing (my/gtd--files-for-week week-start)))
    (cond
     ((null existing)
      (let ((denote-directory dir)
            (denote-kill-buffers nil)
            (title (my/gtd-week-title week-start))
            (date-str (format-time-string "%Y-%m-%d" week-start))
            (body (my/gtd-week-body week-start)))
        (save-window-excursion
          (denote title (list my/gtd-keyword) 'org nil date-str body)
          (save-buffer)
          (buffer-file-name))))
     ((= (length existing) 1)
      (car existing))
     (t
      (let ((default-directory dir))
        (expand-file-name
         (completing-read "Select GTD week file: "
                          (mapcar #'file-name-nondirectory existing)
                          nil t)
         dir))))))

(defun my/gtd--week-file-peek (&optional time)
  "Return path to this week's file if it already exists, else nil.
Never creates.  Used for showing counts in menus without side effects."
  (let* ((week-start (my/gtd-week-start time))
         (existing (my/gtd--files-for-week week-start)))
    (cond
     ((null existing) nil)
     ((= (length existing) 1) (car existing))
     (t (let ((default-directory (my/gtd-ensure-directory)))
          (expand-file-name
           (completing-read "Select GTD week file: "
                            (mapcar #'file-name-nondirectory existing)
                            nil t)
           default-directory))))))

;;;###autoload
(defun my/gtd-open-this-week (&optional time)
  "Visit this week's GTD journal (create if needed).
With prefix arg, prompt for a date in that week."
  (interactive
   (list (when current-prefix-arg
           (org-read-date nil t))))
  (find-file (my/gtd-path-to-new-or-existing time)))

;;;; Navigation inside the week file

(defun my/gtd--goto-headline (level title-regexp)
  "Move to headline at LEVEL matching TITLE-REGEXP.  Return non-nil on success."
  (let ((found nil))
    (goto-char (point-min))
    (while (and (not found)
                (re-search-forward
                 (format "^\\*\\{%d\\} \\(?:\\[[^]\n]*\\] \\)?\\(%s\\)\\s-*$"
                         level title-regexp)
                 nil t))
      (setq found (match-beginning 0)))
    (when found
      (goto-char found)
      (org-back-to-heading t)
      t)))

(defun my/gtd-goto-section (where category)
  "In current buffer, go to WHERE (`today' or `staging') CATEGORY headline."
  (let* ((category (or category my/gtd--capture-category))
         (cat-re (regexp-quote category))
         (parent-re
          (pcase where
            ('staging "Staging")
            ('today (regexp-quote (my/gtd-day-stamp)))
            ((pred stringp) (regexp-quote where))
            (_ (regexp-quote (my/gtd-day-stamp))))))
    (unless (my/gtd--goto-headline 1 parent-re)
      (user-error "GTD: no heading matching %s — is the week template intact?" parent-re))
    (let ((end (save-excursion (org-end-of-subtree t t) (point)))
          (level 2)
          (found nil))
      (while (and (not found)
                  (re-search-forward
                   (format "^\\*\\{%d\\} \\(?:\\[[^]\n]*\\] \\)?%s\\s-*$"
                           level cat-re)
                   end t))
        (setq found (match-beginning 0)))
      (unless found
        (user-error "GTD: no %s under %s" category parent-re))
      (goto-char found)
      (org-back-to-heading t))))

(defun my/gtd-capture-find-target ()
  "Org-capture target function: point on today/staging × category."
  (my/gtd-goto-section my/gtd--capture-where my/gtd--capture-category))

;;;; Stats: done/total + open counts for rich prompts

(defun my/gtd--done-total-under-heading ()
  "Return (DONE . TOTAL) for direct child TODO items under the heading at point."
  (let ((done 0) (total 0)
        (parent-level (org-current-level))
        (end (save-excursion (org-end-of-subtree t t) (point))))
    (save-excursion
      (forward-line 1)
      (while (re-search-forward org-heading-regexp end t)
        (let ((level (org-current-level))
              (todo (org-get-todo-state)))
          (when (and todo (= level (1+ parent-level)))
            (cl-incf total)
            (when (member todo org-done-keywords) (cl-incf done))))))
    (cons done total)))

(defun my/gtd-section-stats (where &optional no-create)
  "Return ((CAT DONE TOTAL) ...) for WHERE.
If NO-CREATE is non-nil, do not create the week file just to read stats."
  (let* ((path (if no-create
                 (my/gtd--week-file-peek)
               (my/gtd-path-to-new-or-existing)))
         res)
    (if (not path)
        ;; no file yet — return zeros for everything, safely
        (setq res (mapcar (lambda (c) (list c 0 0)) my/gtd-categories))
      (with-current-buffer (find-file-noselect path)
        (org-with-wide-buffer
         (dolist (cat my/gtd-categories)
           (save-excursion
             (when (my/gtd-goto-section where cat)
               (let* ((dt (my/gtd--done-total-under-heading))
                      (d (car dt)) (tot (cdr dt)))
                 (push (list cat d tot) res)))))))
      (setq res (nreverse res)))
    res))

(defun my/gtd-open-count (where category &optional no-create)
  "Open (non-done) count for CATEGORY under WHERE.
NO-CREATE: do not create file just to count."
  (let* ((stats (my/gtd-section-stats where no-create))
         (row (cl-find category stats :key #'car :test #'string=)))
    (if row (- (nth 2 row) (nth 1 row)) 0)))

(defun my/gtd-format-where-label (label where)
  "Rich label for where prompt: Today (Core 2/5) etc.
Never creates the week file."
  (let* ((stats (my/gtd-section-stats where t)) ; t = no-create
         (core-row (cl-find "Core" stats :key #'car :test #'string=))
         (c-done (or (nth 1 core-row) 0))
         (c-tot  (or (nth 2 core-row) 0))
         (core-str (format "Core %d/%d" c-done c-tot)))
    (format "%s (%s)" label core-str)))

(defun my/gtd-format-cat-label (cat where)
  "Rich label for category prompt with done/total + Core focus colors.
Never creates the week file."
  (let* ((stats (my/gtd-section-stats where t))
         (row (cl-find cat stats :key #'car :test #'string=))
         (done (or (nth 1 row) 0))
         (tot  (or (nth 2 row) 0))
         (base (format "%s (%d/%d)" cat done tot))
         (open (my/gtd-open-count where cat t))
         (will-be (if (string= cat "Core") (1+ open) open))
         (face
          (cond
           ((and (string= cat "Core")
                 (eq where 'today)
                 (>= will-be 4)) 'error)
           ((and (string= cat "Core")
                 (eq where 'today)
                 (= will-be 3))
            ;; modus-vivendi yellow-ish is often `warning` or `modus-themes-warning`
            'warning)
           (t nil))))
    (if face (propertize base 'face face) base)))

(defun my/gtd-count-core (&optional where)
  "Return number of open Core TODOs for WHERE (compat)."
  (my/gtd-open-count (or where 'today) "Core"))

;;;###autoload
(defun my/gtd-core-status (&optional where)
  "Show open Core count + full stats for the week."
  (interactive)
  (let* ((where (or where 'today))
         (path (my/gtd-path-to-new-or-existing))
         (stats (my/gtd-section-stats where))
         (core-open (my/gtd-open-count where "Core"))
         (label (if (eq where 'staging) "Staging" "Today")))
    (message "%s stats: %s | Core open: %d/%d"
             label
             (mapconcat (lambda (r) (format "%s %d/%d" (nth 0 r) (nth 1 r) (nth 2 r))) stats "  ")
             core-open my/gtd-core-limit)
    (when (and (eq where 'today) (> core-open my/gtd-core-limit))
      (message "⚠ %s Core open %d > limit %d — demote or cut" label core-open my/gtd-core-limit))
    core-open))

(defun my/gtd-warn-core-if-needed ()
  "After capture into Core/today, warn when over `my/gtd-core-limit'."
  (when (and (eq my/gtd--capture-where 'today)
             (string= my/gtd--capture-category "Core"))
    (my/gtd-core-status 'today)))

(defun my/gtd-after-capture ()
  "Refresh cookies and warn about Core overload after a GTD capture."
  (let* ((buf (org-capture-get :buffer))
         (file (or (org-capture-get :file)
                   (and buf (buffer-local-value 'buffer-file-name buf)))))
    (when (and file (file-exists-p file))
      (with-current-buffer (find-file-noselect file)
        (org-update-statistics-cookies t)
        (save-buffer)))
    (my/gtd-warn-core-if-needed)))

;;;; Capture entry points (rich prompts)

(defun my/gtd--capture (where category)
  "Run org-capture template \"gt\" with WHERE and CATEGORY preselected."
  (let ((my/gtd--capture-where where)
        (my/gtd--capture-category category))
    (org-capture nil "gt")))

(defun my/gtd--unified-choices ()
  "Return list of (display . (where . cat)) for the unified SPC n g t prompt.

Candidates look like:
  t Core (2/5)
  t Secondary (1/3)
  s Core (0/1)
  s Unplanned (2/2)

Typing `t ' (t + space) instantly narrows to all Today items.
Typing `s ' (s + space) instantly narrows to all Staging items.

This is extremely fast with consult + orderless.

Only 'Core' items under Today get warning (yellow) or error (red) faces."
  (my/gtd-refresh-key-descriptions)
  (let (res)
    (dolist (where '(today staging))
      (let* ((pfx   (if (eq where 'today) "t " "s "))
             (stats (my/gtd-section-stats where t)))
        (dolist (cat my/gtd-categories)
          (let* ((row  (cl-find cat stats :key #'car :test #'string=))
                 (done (or (nth 1 row) 0))
                 (tot  (or (nth 2 row) 0))
                 (open (my/gtd-open-count where cat t))
                 ;; Color only Today + Core when the count is high
                 (face (cond
                        ((and (eq where 'today)
                              (string= cat "Core")
                              (>= open 3)) 'error)
                        ((and (eq where 'today)
                              (string= cat "Core")
                              (>= open 2)) 'warning)
                        (t nil)))
                 (disp (format "%s%s (%d/%d)" pfx cat done tot)))
            (when face
              (setq disp (propertize disp 'face face)))
            (push (cons disp (cons where cat)) res)))))
    (nreverse res)))

(defun my/gtd--resolve (sel table)
  "Return (where . cat) for SEL, ignoring any text properties (faces)."
  (let ((plain (substring-no-properties sel)))
    (cdr (seq-find (lambda (e) (string= plain (substring-no-properties (car e))))
                   table))))

;;;###autoload
(defun my/gtd-capture-task ()
  "Unified prompt: Today + Staging + all categories in one list.

Filtering:
  `t '   → narrow to Today items
  `s '   → narrow to Staging items

Works great with consult + orderless.

Core on Today gets warning (yellow) at 2 open, error (red) at 3+.

Bound to SPC n g t."
  (interactive)
  (my/gtd-refresh-key-descriptions)
  (let* ((table (my/gtd--unified-choices))   ; ((display-string . (where . cat)) ...)
         (cands (mapcar #'car table))
         (sel (if (fboundp 'consult--read)
                  (consult--read cands
                                 :prompt "GTD (t SPC = Today, s SPC = Staging): "
                                 :require-match t
                                 :category 'gtd-task)
                (completing-read "GTD (t / s prefix): " cands nil t)))
         (pair (my/gtd--resolve sel table))
         (where (car pair))
         (cat   (cdr pair)))
    (unless (and where cat)
      (user-error "GTD: could not resolve selection"))
    (my/gtd--capture where cat)))

;;;###autoload
(defun my/gtd-capture-today-core ()
  "Capture a Core task under today."
  (interactive)
  (my/gtd--capture 'today "Core"))

;;;###autoload
(defun my/gtd-capture-today-secondary ()
  "Capture a Secondary task under today."
  (interactive)
  (my/gtd--capture 'today "Secondary"))

;;;###autoload
(defun my/gtd-capture-today-unplanned ()
  "Capture an Unplanned task under today."
  (interactive)
  (my/gtd--capture 'today "Unplanned"))

;;;###autoload
(defun my/gtd-capture-staging-core ()
  "Capture a Core task under Staging."
  (interactive)
  (my/gtd--capture 'staging "Core"))

;;;###autoload
(defun my/gtd-capture-staging-secondary ()
  "Capture a Secondary task under Staging."
  (interactive)
  (my/gtd--capture 'staging "Secondary"))

;;;###autoload
(defun my/gtd-capture-staging-unplanned ()
  "Capture an Unplanned task under Staging."
  (interactive)
  (my/gtd--capture 'staging "Unplanned"))

;;;; Org-capture template list (merged by config)

(defun my/gtd--capture-desc (prefix where cat)
  "Build a description string like \"Core today (2/5)\".
Safe: does not create the week file."
  (let* ((stats (my/gtd-section-stats where t))
         (row (cl-find cat stats :key #'car :test #'string=))
         (done (or (nth 1 row) 0))
         (tot (or (nth 2 row) 0))
         (loc (if (eq where 'staging) "staging" "today")))
    (format "%s %s (%d/%d)" prefix loc done tot)))

(defun my/gtd-org-capture-templates ()
  "Return GTD-related `org-capture-templates' entries with live counts in descriptions."
  (let ((today-core (my/gtd--capture-desc "Core" 'today "Core"))
        (today-sec  (my/gtd--capture-desc "Secondary" 'today "Secondary"))
        (today-unp  (my/gtd--capture-desc "Unplanned" 'today "Unplanned"))
        (stag-core  (my/gtd--capture-desc "Core" 'staging "Core"))
        (stag-sec   (my/gtd--capture-desc "Secondary" 'staging "Secondary"))
        (stag-unp   (my/gtd--capture-desc "Unplanned" 'staging "Unplanned")))
    `(("g" "GTD weekly")
      ("gt" "GTD task (prompt with counts)" entry
       (file+function my/gtd-path-to-new-or-existing my/gtd-capture-find-target)
       "*** TODO %?\n" :empty-lines 0 :after-finalize my/gtd-after-capture)
      ("gc" ,today-core entry
       (file+function my/gtd-path-to-new-or-existing
                      (lambda () (setq my/gtd--capture-where 'today my/gtd--capture-category "Core")
                              (my/gtd-capture-find-target)))
       "*** TODO %?\n" :empty-lines 0 :after-finalize my/gtd-after-capture)
      ("gs" ,today-sec entry
       (file+function my/gtd-path-to-new-or-existing
                      (lambda () (setq my/gtd--capture-where 'today my/gtd--capture-category "Secondary")
                              (my/gtd-capture-find-target)))
       "*** TODO %?\n" :empty-lines 0 :after-finalize my/gtd-after-capture)
      ("gu" ,today-unp entry
       (file+function my/gtd-path-to-new-or-existing
                      (lambda () (setq my/gtd--capture-where 'today my/gtd--capture-category "Unplanned")
                              (my/gtd-capture-find-target)))
       "*** TODO %?\n" :empty-lines 0 :after-finalize my/gtd-after-capture)
      ("gC" ,stag-core entry
       (file+function my/gtd-path-to-new-or-existing
                      (lambda () (setq my/gtd--capture-where 'staging my/gtd--capture-category "Core")
                              (my/gtd-capture-find-target)))
       "*** TODO %?\n" :empty-lines 0 :after-finalize my/gtd-after-capture)
      ("gS" ,stag-sec entry
       (file+function my/gtd-path-to-new-or-existing
                      (lambda () (setq my/gtd--capture-where 'staging my/gtd--capture-category "Secondary")
                              (my/gtd-capture-find-target)))
       "*** TODO %?\n" :empty-lines 0 :after-finalize my/gtd-after-capture)
      ("gU" ,stag-unp entry
       (file+function my/gtd-path-to-new-or-existing
                      (lambda () (setq my/gtd--capture-where 'staging my/gtd--capture-category "Unplanned")
                              (my/gtd-capture-find-target)))
       "*** TODO %?\n" :empty-lines 0 :after-finalize my/gtd-after-capture))))

;;;###autoload
(defun my/gtd-insert-week-template ()
  "Insert a full Sat–Fri + Staging week body at point (for tempel/yasnippet parity)."
  (interactive)
  (insert (my/gtd-week-body (my/gtd-week-start))))

;;;; Dynamic template refresh so org-capture menu shows live counts

(defun my/gtd-refresh-capture-templates ()
  "Rebuild GTD capture templates with current done/total counts.
Call this before showing the GTD capture menu."
  (let* ((base (cl-remove-if
                (lambda (tpl)
                  (and (listp tpl) (stringp (car tpl))
                       (string-prefix-p "g" (car tpl))))
                org-capture-templates))
         (fresh (my/gtd-org-capture-templates)))
    (setq org-capture-templates (append base fresh))))

(defun my/gtd-org-capture (&optional keys)
  "Like org-capture but ensures GTD submenu has fresh counts."
  (interactive)
  (my/gtd-refresh-capture-templates)
  (org-capture nil keys))

;; Make sure even the normal C-c c g path shows live counts
(advice-add 'org-capture :before
            (lambda (&rest _)
              (when (and (boundp 'org-capture-templates)
                       (fboundp 'my/gtd-refresh-capture-templates))
                ;; Only refresh if the GTD "g" submenu exists
                (when (cl-find-if (lambda (x) (and (listp x) (equal (car x) "g")))
                                  org-capture-templates)
                  (my/gtd-refresh-capture-templates))))
            '((name . gtd-live-counts)))

(provide 'gtd-weekly)
;;; gtd-weekly.el ends here
