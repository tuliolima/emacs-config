(use-package tagedit
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;; you need to install the npm package!
;; $ sudo npm install -g livedown
;; see https://github.com/shime/emacs-livedown
(use-package livedown
  :ensure t
  :straight (:host github :repo "shime/emacs-livedown" :files ("livedown.el")))
