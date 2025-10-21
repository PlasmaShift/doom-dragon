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
(setopt doom-theme 'modus-vivendi)

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
(setq org-directory "~/org/")


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

(use-package! denote
  :demand t
  :config
  (cond
   ((string-match-p "travi" user-login-name) ;; Travis windows Computer with an Rusty Drive
    (setopt denote-directory (expand-file-name "R:/docs/denote/denote")))
   ((string-match-p "travi" user-login-name) ;; Matts Windows computer It conat be ~/doc Damm windows
    (setopt denote-directory (expand-file-name "c:/Users/Plasma/denote/denote/")))
   (t (setopt denote-directory (expand-file-name "~/doc/denote/denote")))))


;; (use-package consult-project-extra
;;   :ensure (consult-project-extra :type git :host github :repo "Qkessler/consult-project-extra")
;;   :bind
;;   (("C-c p f" . consult-project-extra-find)
;;    ("C-c p o" . consult-project-extra-find-other-window)))

(append-message-to-init-config-debug "denote Loaded")

(use-package! consult-notes
  :demand t
  :after denote
  :init
  (setq consult-notes-file-dir-sources
  	`(("Denote Notes"  ?d ,(denote-directory))
  	  )))

(use-package! consult-denote
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode 1)
  (blackout 'consult-denote-mode))

(append-message-to-init-config-debug "Config finished")
