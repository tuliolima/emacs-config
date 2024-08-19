(add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)

(use-package magit
  :ensure t
  :bind ("C-M-;" . magit-status) )
