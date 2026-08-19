;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages.el whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)

(package! aidermacs :recipe (:host github :repo "MatthewZMD/aidermacs"))
(package! exec-path-from-shell)
(package! no-littering)
(package! dired-preview)
(package! telega)
(package! vdf-mode)
(package! denote)
(package! denote-org)
;; (package! denote-sequence)
(package! denote-journal)
(package! consult-denote)
(package! tempel)
(package! tempel-collection)
(package! substitute)
;; Disabled: savannah hyperbole checkout fails (git cat-file / bad object 907ef24)
;; (package! hyperbole)
;; (package! )
;; (package! )
;; (package! )
;; (package! )
;; 
(package! denote-silo)
(package! denote-search)
;; (package! denote-explore)
(package! citar-denote)
(package! consult-notes)
(package! denote-menu)
(package! eldoc-box)
(package! epkg)
(package! kdl-mode)
(package! dired-rsync)
;; (package! emacs-reader)
(package! calfw)
;; (package! calfw-blocks)
;; (package! calfw-blocks
;;   :recipe (:host github :repo haji-ali/calfw-blocks
;;            :pin "87937b7c7523b6d314bf9e21310924c94f182954"))

;; (package! calfw-blocks
;;   :recipe (:host github
;;            :repo "haji-ali/calfw-blocks"
;;            :commit "87937b7c7523b6d314bf9e21310924c94f182954"))

;; (package! calfw-blocks
;;   :recipe (:pin  "https://https://github.com/haji-ali/calfw-blocks" "87937b7c7523b6d314bf9e21310924c94f182954"))
;; ;; (package! reader  ; or emacs-reader
;;   :recipe (:host codeberg :repo divyaranjan/emacs-reader))

;; (package! org-window-habit)
(package! diredfl
  :disable t)
;; (package! org-appear
;;   :disable t)
;; (package! org-expose-emphasis-markers)

(package! notmuch)
(package! notmuch-indicator)
(package! ol-notmuch)
(package! notmuch-transient)
(package! notmuch-bookmarks)
;; (package! notmuch-addr)
;; Disabled: codeberg.org/jao/consult-notmuch.git returns 503 (breaks nix-doom fetch)
;; (package! consult-notmuch)
(package! activities)
(package! org-msg)
(package! msgpack)
;; (package! tramp-rpc :recipe (:host github :repo “ArthurHeymans/emacs-tramp-rpc”))
;; (package! piem)
(package! eca :recipe (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el")))
(package! mcp-server
  :recipe (:host github :repo "rhblind/emacs-mcp-server"
           :files ("*.el" "tools/*.el" "mcp-wrapper.py" "mcp-wrapper.sh"))
  :pin "a5d749cf9880598f66308545985526fd4460627f")
(package! shell-maker)
(package! acp)
(package! agent-shell)
(package! pi-coding-agent)
(package! fj
  :recipe (:host codeberg :repo "martianh/fj.el"))
(package! fedi
  :recipe (:host codeberg :repo "martianh/fedi.el"))
(package! tp
  :recipe (:host codeberg :repo "martianh/tp.el"))
;; (package! elfeed)
;; (package! cockoo-search)
