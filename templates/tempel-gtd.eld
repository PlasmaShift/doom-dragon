;; Tempel templates for weekly GTD (org-mode)
;; Loaded via tempel-path from doom-dragon config.

org-mode

;; Quick task line (when already under Core/Secondary/Unplanned)
(gtdt "*** TODO " p n)

;; One day block (Sat–Fri style headings)
(gtdd "* <" (format-time-string "%Y-%m-%d %a") ">" n
      "** [/] Core" n
      "** [/] Secondary" n
      "** [/] Unplanned" n)

;; Full week body (Saturday start) — calls your helper if loaded
(gtdw (my/gtd-week-body (my/gtd-week-start)))

;; Staging block only
(gtds "* Staging" n
      "** [/] Core" n
      "** [/] Secondary" n
      "** [/] Unplanned" n)
