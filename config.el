;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setopt doom-theme 'doom-one)
;; (setopt doom-theme 'modus-vivendi)
(setq doom-theme 'modus-vivendi)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setopt display-line-numbers-type t)

(defun append-message-to-init-config-debug (string)
  "Append a message to a new line in the '*init-config-debug*' buffer."
  (save-window-excursion
    (if (get-buffer "*init-config-debug*")
        (progn
          (with-current-buffer (get-buffer "*init-config-debug*")
            (goto-char (point-max))
            (newline)
            (insert string)))
      (progn
        (switch-to-buffer (get-buffer-create "*init-config-debug*"))
        (insert string)))))

(append-message-to-init-config-debug "Config loaded")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setopt org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(exec-path-from-shell-initialize)

;; user-emacs-directory "/home/plasmastrike/.cache/doom/nix" why

;; (use-package no-littering
;;   :demand t
;;   :config
;;   (require 'no-littering))

(use-package custom
  :ensure nil
  :no-require t
  :config
  ;; (setopt custom-file (expand-file-name "custom.el" user-emacs-directory)) :; TODO fix user-emacs-directory
  (setopt custom-file (expand-file-name "custom.el" "~/.config/doom-dragon/"))
  (when (file-exists-p custom-file)
    (load custom-file)))


;; Indented paste helpers (body was previously outside the defuns — fixed)
(defun +evil/paste-indent-after (count &optional register)
  "Paste after on a new line and auto-indent.
COUNT is how many times to paste. REGISTER is optional."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (when (and current-prefix-arg (not (numberp current-prefix-arg)))
           evil-this-register)))
  (evil-append-line 1)
  (evil-paste-after count (or register ?\"))
  (evil-indent (evil-get-marker-position ?\[)
               (evil-get-marker-position ?\])))

(defun +evil/paste-indent-before (count &optional register)
  "Paste before on a new line and auto-indent.
COUNT is how many times to paste. REGISTER is optional."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (when (and current-prefix-arg (not (numberp current-prefix-arg)))
           evil-this-register)))
  (evil-open-above 1)
  (evil-paste-before count (or register ?\"))
  (evil-indent (evil-get-marker-position ?\[)
               (evil-get-marker-position ?\])))

(defun +evil/indent-region-or-last-paste (count)
  "Indent current region or last pasted text.
With prefix argument COUNT, indent that many lines.
If region is active, indents the region. Otherwise indents last paste."
  (interactive "p")
  (if (use-region-p)
      (evil-indent (region-beginning) (region-end))
    (evil-indent (evil-get-marker-position ?\[)
                 (evil-get-marker-position ?\]))))

(map! :n "]p" #'+evil/paste-indent-after
      :n "[p" #'+evil/paste-indent-before
      :n "SPC p =" #'+evil/indent-region-or-last-paste)

(use-package! vdf-mode
  :mode "\\.vdf\\'"
  :hook (vdf-mode . (lambda () (setq tab-width 4))))

(use-package! kdl-mode
  :mode "\\.kdl\\'")


;; (use-package! aidermacs
;;   :config
;;   ;; Set default model to attempt Kagi FastGPT (may require Aider config tweaks)
;;   (setq aidermacs-default-model "fastgpt")
;;   ;; Enable Aidermacs mode globally (or per buffer as needed)
;;   (aidermacs-mode 1)
;;   ;; Optional: Custom function to run Aidermacs with Kagi backend
;;   (defun my-aidermacs-with-kagi ()
;;     (interactive)
;;     (let ((gptel-backend (gptel-get-backend "Kagi")))
;;       (aidermacs-run-command)))
;;   ;; Bind to a key if desired, e.g., (map! :leader "a k" #'my-aidermacs-with-kagi)
;;   )

(use-package! gptel
  :config
  ;; Set up Kagi FastGPT backend using env var
  (gptel-make-kagi "Kagi" :key (lambda () (getenv "KAGI_API_KEY")))

  ;; Set up Gemini backend using env var
  (gptel-make-gemini "Gemini" :key (getenv "GEMINI_API_KEY") :stream t)

  ;; Set Kagi FastGPT as the default backend
  (setopt gptel-backend (gptel-get-backend "Kagi"))
  (setopt gptel-default-mode 'org-mode)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")

  ;; Optional: Enable gptel mode for keybindings
  ;; (gptel-mode 1)
  )
(append-message-to-init-config-debug "aider loaded")

(use-package! eldoc-box
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))

;; (use-package! denote
;;   :demand t
;;   :custom
;;   (denote-sort-keywords t)
;;   :hook
;;   (dired-mode . denote-dired-mode)
;;   :config
;;   (cond
;;    ((string-match-p "travi" user-login-name) ;; Travis windows Computer with an Rusty Drive
;;     (setopt denote-directory (expand-file-name "R:/docs/denote/denote")))
;;    ((string-match-p "travi" user-login-name) ;; Matts Windows computer It conat be ~/doc Damm windows
;;     (setopt denote-directory (expand-file-name "c:/Users/Plasma/denote/denote/")))
;;    (t (setopt denote-directory (expand-file-name "~/doc/denote/denote")))))


(use-package dired-rsync)


(use-package! denote
  :demand t
  :custom
  (denote-sort-keywords t)
  :hook
  (dired-mode . denote-dired-mode)
  :config
  ;; Prefer system-type so "plasmastrike" on Linux is not mistaken for Windows "Plasma".
  (cond
   ((and (eq system-type 'windows-nt)
         (string-match-p "travi" user-login-name)) ;; Travis Windows + Rusty Drive
    (setopt denote-directory (expand-file-name "R:/docs/denote/denote")))
   ((eq system-type 'windows-nt) ;; Matt / other Windows
    (setopt denote-directory (expand-file-name "c:/Users/Plasma/denote/denote/")))
   (t (setopt denote-directory (expand-file-name "~/doc/denote/denote")))))

;; General daily journal (separate from weekly GTD)
(use-package! denote-journal
  :after denote
  :demand t
  :config
  (setopt denote-journal-title-format 'day
          denote-journal-interval 'daily
          ;; hook runs AFTER a new entry is created — do not call new-or-existing here
          denote-journal-hook nil)
  (cond
   ((and (eq system-type 'windows-nt)
         (string-match-p "travi" user-login-name))
    (setopt denote-journal-directory (expand-file-name "R:/docs/denote/journal")))
   ((eq system-type 'windows-nt)
    (setopt denote-journal-directory (expand-file-name "c:/Users/Plasma/denote/journal")))
   (t (setopt denote-journal-directory (expand-file-name "~/doc/denote/journal")))))

;; Weekly GTD (Saturday–Friday) — local module, not a package
(after! denote
  (load! "lisp/gtd-weekly")
  (cond
   ((and (eq system-type 'windows-nt)
         (string-match-p "travi" user-login-name))
    (setopt my/gtd-directory (expand-file-name "R:/docs/denote/gtd")))
   ((eq system-type 'windows-nt)
    (setopt my/gtd-directory (expand-file-name "c:/Users/Plasma/denote/gtd")))
   (t (setopt my/gtd-directory (expand-file-name "~/doc/denote/gtd"))))
  (my/gtd-ensure-directory))

;; Tempel (compare with Doom's built-in yasnippet under snippets/)
(use-package! tempel
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))
  :init
  (setq tempel-path
        (list (expand-file-name "templates/*.eld" doom-user-dir)))
  :config
  (defun my/tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand completion-at-point-functions)))
  (add-hook 'conf-mode-hook #'my/tempel-setup-capf)
  (add-hook 'prog-mode-hook #'my/tempel-setup-capf)
  (add-hook 'text-mode-hook #'my/tempel-setup-capf))

(use-package! tempel-collection
  :after tempel)

(after! org-capture
  ;; One place for all templates (avoid setopt then add-to-list fighting)
  (setopt org-capture-templates
          (append
           '(("d" "Denote: New note (create or edit)" plain
              (function denote-org-capture)
              "%?" :empty-lines 1)
             ("j" "Denote: Journal entry (append timed heading)" entry
              (file+function denote-journal-path-to-new-or-existing-entry
                             (lambda ()
                               (goto-char (point-max))
                               (unless (bolp) (newline))))
              "* [%H:%M] %(read-string \"Entry title: \")\n%?\n%U"
              :empty-lines 1)
             ("k" "Journal (simple)" entry
              (file denote-journal-path-to-new-or-existing-entry)
              "* %U %?\n%i\n%a"
              :kill-buffer t
              :empty-lines 1))
           (my/gtd-org-capture-templates)))
  ;; Ensure the GTD submenu descriptions start with real counts (refreshed again on SPC n g x)
  (when (fboundp 'my/gtd-refresh-capture-templates)
    (my/gtd-refresh-capture-templates)))

;; Denote notes
(map! :leader
      (:prefix ("n d" . "Denote")
       :desc "Create/edit note" "c" #'denote-org-capture
       :desc "Journal entry"    "j" (cmd! (org-capture nil "j"))
       :desc "Find note"        "f" #'consult-denote-find
       :desc "Grep in notes"    "g" #'consult-denote-grep
       :desc "Insert link"      "l" #'denote-link
       :desc "Backlinks"        "b" #'denote-backlinks))

;; Weekly GTD
(map! :leader
      (:prefix ("n g" . "GTD week")
       :desc "Open this week"           "g" #'my/gtd-open-this-week
       :desc "Capture task (prompt with counts)" "t" #'my/gtd-capture-task
       :desc "Capture Core today"       "c" #'my/gtd-capture-today-core
       :desc "Capture Secondary today"  "s" #'my/gtd-capture-today-secondary
       :desc "Capture Unplanned today"  "u" #'my/gtd-capture-today-unplanned
       :desc "Capture Core staging"     "C" #'my/gtd-capture-staging-core
       :desc "Capture Secondary staging" "S" #'my/gtd-capture-staging-secondary
       :desc "Capture Unplanned staging" "U" #'my/gtd-capture-staging-unplanned
       :desc "Core task status"         "n" #'my/gtd-core-status
       :desc "Org-capture GTD menu (live counts)" "x" (cmd! (my/gtd-org-capture "g"))))






;; (require 'calfw-org)
;; (require 'calfw-blocks)


;; (use-package! denote
;;   :demand t  ;; Force-load Denote immediately to avoid commandp errors
;;   :custom
;;   (denote-sort-keywords t)
;;   :hook
;;   (dired-mode . denote-dired-mode)
;;   :config
;;   (cond
;;    ((string-match-p "travi" user-login-name) ;; Travis windows Computer with an Rusty Drive
;;     (setopt denote-directory (expand-file-name "R:/docs/denote/denote")))
;;    ((string-match-p "travi" user-login-name) ;; Matts Windows computer It conat be ~/doc Damm windows
;;     (setopt denote-directory (expand-file-name "c:/Users/Plasma/denote/denote/")))
;;    (t (setopt denote-directory (expand-file-name "~/doc/denote/denote")))))

;; ;; Separate block for Denote Journal (as requested)
;; (use-package! denote-journal
;;   :after denote  ;; Ensure journal loads after main Denote to avoid issues
;;   :demand t  ;; Force-load to make functions available immediately
;;   :config
;;   ;; Journal-specific settings: Use daily journals with timed headings
;;   (setopt denote-journal-title-format 'day)  ;; Journals named by date (e.g., 20240920.org)
;;   (setopt denote-journal-hook '(denote-journal-new-or-existing-entry))  ;; Auto-create if needed
;;   ;; Set journal directory conditionally, matching denote-directory (with your comments)
;;   (cond
;;    ((string-match-p "travi" user-login-name) ;; Travis windows Computer with an Rusty Drive
;;     (setopt denote-journal-directory (expand-file-name "R:/docs/denote/journal")))  ;; Journal as subdir (adjust if needed)
;;    ((string-match-p "travi" user-login-name) ;; Matts Windows computer It conat be ~/doc Damm windows
;;     (setopt denote-journal-directory (expand-file-name "c:/Users/Plasma/denote/journal")))  ;; Journal as subdir (adjust if needed)
;;    (t (setopt denote-journal-directory (expand-file-name "~/doc/denote/journal")))))  ;; Journal as subdir (adjust if needed)

;; (require 'denote)
;; ;; Org-capture templates for Denote (inspired by Prot's denote-org-capture)

;; (after! org-capture
;;   (setopt org-capture-templates
;;           '(("d" "Denote: New note (create or edit)" plain  ;; Use 'plain' for Denote's dynamic note creation
;;              (function denote-org-capture)  ;; Calls Denote's function to create/edit dynamically
;;              "%?" :empty-lines 1)  ;; Simple template: prompts for title/keywords, adds content
;;             ("j" "Denote: Journal entry (append timed heading)" entry
;;              (file+function (lambda () (denote-journal-new-or-existing-entry))  ;; Get/create today's journal file in journal dir
;;                             (lambda () (goto-char (point-max)) (unless (bolp) (newline))))  ;; Position at end for append (hides rest of journal during capture)
;;              "* [%H:%M] %(read-string \"Entry title: \")\n%?\n%U" :empty-lines 1))))  ;; Timed heading + content

;; ;; ... (your other use-package! blocks, e.g., consult-notes, consult-denote)

;; ;; Denote bindings (mimicking Doom's org-roam style under SPC n d, but for Denote)
;; ;; Placed outside after! for immediate evaluation; overwrites org-roam bindings
;; (map! :leader
;;       (:prefix ("n d" . "Denote")
;;                "c" #'denote-org-capture               :desc "Create/edit note"      ;; SPC n d c: New/edit note
;;                "j" (lambda () (interactive) (org-capture nil "j")) :desc "Journal entry"        ;; SPC n d j: Append timed journal entry
;;                "f" #'consult-denote-find              :desc "Find note"             ;; SPC n d f: Search notes
;;                "g" #'consult-denote-grep              :desc "Grep in notes"         ;; SPC n d g: Grep search
;;                "l" #'denote-link                      :desc "Insert link"           ;; SPC n d l: Insert link to note
;;                "b" #'denote-backlinks                 :desc "Backlinks"))           ;; SPC n d b: Show backlinks

;; ;; Duplicate bindings with Niri popup (commented out; uncomment to use)
;; ;; Uses emacsclient for a floating frame (as per Prot's blog)
;; ;; (map! :leader
;; ;;       (:prefix ("n d" . "Denote")
;; ;;        "c" (lambda () (interactive) (call-process "emacsclient" nil nil nil "-c" "-e" "(denote-org-capture)")) :desc "Create/edit note"      ;; SPC n d c: Popup for new/edit note
;; ;;        "j" (lambda () (interactive) (call-process "emacsclient" nil nil nil "-c" "-e" "(org-capture nil \"j\")")) :desc "Journal entry"        ;; SPC n d j: Append timed journal entry
;; ;;        "f" #'consult-denote-find              :desc "Find note"             ;; SPC n d f: Search notes (no popup needed)
;; ;;        "g" #'consult-denote-grep              :desc "Grep in notes"         ;; SPC n d g: Grep search (no popup needed)
;; ;;        "l" #'denote-link                      :desc "Insert link"           ;; SPC n d l: Insert link (no popup needed)
;; ;;        "b" #'denote-backlinks                 :desc "Backlinks"))           ;; SPC n d b: Show backlinks (no popup needed)





;; (use-package consult-project-extra
;;   :ensure (consult-project-extra :type git :host github :repo "Qkessler/consult-project-extra")
;;   :bind
;;   (("C-c p f" . consult-project-extra-find)
;;    ("C-c p o" . consult-project-extra-find-other-window)))

(append-message-to-init-config-debug "denote Loaded")

(use-package! consult-notes
  :demand t
  :after (denote denote-journal)
  :config
  (setopt consult-notes-file-dir-sources
          `(("Denote Notes" ?d ,(denote-directory))
            ("GTD weeks"    ?g ,(if (boundp 'my/gtd-directory)
                                    (expand-file-name my/gtd-directory)
                                  (expand-file-name "~/doc/denote/gtd")))
            ("Journal"      ?j ,(denote-journal-directory)))))

(use-package! consult-denote
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode 1)
  ;; (blackout 'consult-denote-mode))
  )

;; (use-package! org-expose-emphasis-markers
;;   ;; :hook
;;   ;; (org-mode . )
;;   :config
;;   ;; 1. make sure `org-hide-emphasis-markers' is true
;;   (setopt org-hide-emphasis-markers t)

;;   ;; 2. (optional) set the exposing scope, default value is 'item
;;   (setopt org-expose-emphasis-markers-type 'paragraph)
;;   (add-hook 'org-mode-hook (lambda () (org-expose-emphasis-markers 'paragraph)))
;;   ;; 3. turn on the mode
;;   ;; (add-hook 'org-mode-hook (lambda () (org-expose-emphasis-markers-mode t)))
;;   )

(require 'notmuch-address)
(setq notmuch-address-command "/usr/local/bin/notmuch-addrlookup")
(notmuch-address-message-insinuate)
(setq mail-user-agent 'notmuch-user-agent) ;; message-user-agent default

(setq-default notmuch-search-oldest-first nil)
;; (setq sendmail-program "mujmap")
(setq sendmail-program "mujmap")
(setq message-send-mail-function #'message-send-mail-with-sendmail)
(setq message-send-mail-function #'message-send-mail-with-sendmail)
(setq message-sendmail-extra-arguments '("-C" "/home/plasmastrike/mail/account.fastmail/mujmap" "send")) ;;TODO remove home dir

(setq notmuch-always-prompt-for-sender nil)
(setq user-mail-address "plasmastrike@voiddragon.me")
;; (setq user-full-name "plasmastrike@voiddragon.me")
(setq user-full-name "Plasma Strike")

;; (setq sendmail-program "/home/plasmastrike/.local/bin/gmi")
;; (setq message-sendmail-extra-arguments '("send" "--quiet" "-t" "-C" "~/mail/account.gmail"))

;; ;; (setq message-sendmail-extra-arguments '("send" "--quiet" "-t" "-C" "/home/plasmastrike/mail/account.gmail"))
;;   ;; Optional: Don't save outgoing mail locally.
;; (setq notmuch-fcc-dirs nil)


(setq send-mail-function 'sendmail-send-it
      ;; sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)

(setq notmuch-fcc-dirs nil)

(append-message-to-init-config-debug "email configured")

(append-message-to-init-config-debug "Config finished")
