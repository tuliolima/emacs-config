;; Lots of great themes, both light ones
;; and dark ones. Use M-x load-theme to select one.
;; The first time you load one, it asks for
;; confirmation. You can see what they all
;; look like here:
;; https://github.com/doomemacs/themes/tree/screenshots
(setup (:package doom-themes)
  (when (not custom-enabled-themes)
    (load-theme 'doom-solarized-light t)))

(use-package auto-dark
  :ensure t
  :config
  (auto-dark-mode t)
  :custom
  (auto-dark-light-theme 'doom-solarized-light)
  (auto-dark-dark-theme 'doom-solarized-dark))
