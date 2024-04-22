(use-package yaml-mode
  :ensure t
  :hook (yaml-mode .)
  :bind (:map yaml-mode-map
         ("\C-m" . 'newline-and-indent)))
