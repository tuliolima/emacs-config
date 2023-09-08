;; projectile is another amazing package from the
;; creator of CIDER. It's got lots of commands
;; for searching and managing files in a project.
;; https://projectile.mx/
(setup (:package projectile)
  (projectile-mode +1)
  (:bind "C-p" projectile-command-map
         "C-c p" projectile-command-map))

;; counsel-projectile integrates projectile with
;; counsel's browse-and-select UI
(setup (:package counsel-projectile))

(setq projectile-switch-project-action 'projectile-dired)
