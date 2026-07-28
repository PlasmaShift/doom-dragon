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

(defcustom my/gtd-keyword "gtd"
  "Denote keyword for weekly GTD files."
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

(defun my/gtd-path-to-new-or-existing (&optional time)
  "Return path to this week's GTD file, creating it if missing.
TIME selects which week (default now).  Identifier date is the Saturday."
  (let* ((week-start (my/gtd-week-start time))
         (dir (my/gtd-ensure-directory))
         (existing (my/gtd--files-for-week week-start)))
    (cond
     ((null existing)
      (let ((denote-directory dir)
            (denote-kill-buffers nil)
            (title (format-time-string "GTD week of %Y-%m-%d (Sat–Fri)" week-start))
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

;;;; Core task counting

(defun my/gtd--count-todos-under-point ()
  "Count open TODO headlines that are direct children of heading at point."
  (let ((count 0)
        (parent-level (org-current-level))
        (end (save-excursion (org-end-of-subtree t t) (point))))
    (save-excursion
      (forward-line 1)
      (while (re-search-forward org-heading-regexp end t)
        (let ((level (org-current-level))
              (todo (org-get-todo-state)))
          (when (and todo
                     (= level (1+ parent-level))
                     (not (member todo org-done-keywords)))
            (cl-incf count)))))
    count))

(defun my/gtd-count-core (&optional where)
  "Return number of open Core TODOs for WHERE (`today' or `staging')."
  (let ((where (or where 'today))
        (path (my/gtd-path-to-new-or-existing)))
    (with-current-buffer (find-file-noselect path)
      (org-with-wide-buffer
       (save-excursion
         (my/gtd-goto-section where "Core")
         (my/gtd--count-todos-under-point))))))

;;;###autoload
(defun my/gtd-core-status (&optional where)
  "Message open Core task count for WHERE (default today) and warn if over limit."
  (interactive)
  (let* ((where (or where 'today))
         (n (my/gtd-count-core where))
         (label (if (eq where 'staging) "Staging" "Today")))
    (if (> n my/gtd-core-limit)
        (message "⚠ %s Core: %d open (limit %d) — demote to Secondary or cut"
                 label n my/gtd-core-limit)
      (message "%s Core: %d / %d open"
               label n my/gtd-core-limit))
    n))

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

;;;; Capture entry points

(defun my/gtd--capture (where category)
  "Run org-capture template \"gt\" with WHERE and CATEGORY preselected."
  (let ((my/gtd--capture-where where)
        (my/gtd--capture-category category))
    (org-capture nil "gt")))

;;;###autoload
(defun my/gtd-capture-task ()
  "Prompt for Today/Staging and Core/Secondary/Unplanned, then capture."
  (interactive)
  (let* ((where-choice
          (completing-read "Where: " '("Today" "Staging") nil t nil nil "Today"))
         (cat (completing-read "Category: " my/gtd-categories nil t nil nil "Core"))
         (where (if (string= where-choice "Staging") 'staging 'today)))
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

(defun my/gtd-org-capture-templates ()
  "Return GTD-related `org-capture-templates' entries."
  '(("g" "GTD weekly")
    ("gt" "GTD task (preselected where/category)" entry
     (file+function my/gtd-path-to-new-or-existing my/gtd-capture-find-target)
     "*** TODO %?\n"
     :empty-lines 0
     :after-finalize my/gtd-after-capture)
    ("gc" "GTD Core → today" entry
     (file+function my/gtd-path-to-new-or-existing
                    (lambda ()
                      (setq my/gtd--capture-where 'today
                            my/gtd--capture-category "Core")
                      (my/gtd-capture-find-target)))
     "*** TODO %?\n"
     :empty-lines 0
     :after-finalize my/gtd-after-capture)
    ("gs" "GTD Secondary → today" entry
     (file+function my/gtd-path-to-new-or-existing
                    (lambda ()
                      (setq my/gtd--capture-where 'today
                            my/gtd--capture-category "Secondary")
                      (my/gtd-capture-find-target)))
     "*** TODO %?\n"
     :empty-lines 0
     :after-finalize my/gtd-after-capture)
    ("gu" "GTD Unplanned → today" entry
     (file+function my/gtd-path-to-new-or-existing
                    (lambda ()
                      (setq my/gtd--capture-where 'today
                            my/gtd--capture-category "Unplanned")
                      (my/gtd-capture-find-target)))
     "*** TODO %?\n"
     :empty-lines 0
     :after-finalize my/gtd-after-capture)
    ("gC" "GTD Core → staging" entry
     (file+function my/gtd-path-to-new-or-existing
                    (lambda ()
                      (setq my/gtd--capture-where 'staging
                            my/gtd--capture-category "Core")
                      (my/gtd-capture-find-target)))
     "*** TODO %?\n"
     :empty-lines 0
     :after-finalize my/gtd-after-capture)
    ("gS" "GTD Secondary → staging" entry
     (file+function my/gtd-path-to-new-or-existing
                    (lambda ()
                      (setq my/gtd--capture-where 'staging
                            my/gtd--capture-category "Secondary")
                      (my/gtd-capture-find-target)))
     "*** TODO %?\n"
     :empty-lines 0
     :after-finalize my/gtd-after-capture)
    ("gU" "GTD Unplanned → staging" entry
     (file+function my/gtd-path-to-new-or-existing
                    (lambda ()
                      (setq my/gtd--capture-where 'staging
                            my/gtd--capture-category "Unplanned")
                      (my/gtd-capture-find-target)))
     "*** TODO %?\n"
     :empty-lines 0
     :after-finalize my/gtd-after-capture)))

;;;###autoload
(defun my/gtd-insert-week-template ()
  "Insert a full Sat–Fri + Staging week body at point (for tempel/yasnippet parity)."
  (interactive)
  (insert (my/gtd-week-body (my/gtd-week-start))))

(provide 'gtd-weekly)
;;; gtd-weekly.el ends here
