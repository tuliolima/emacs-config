(define-key dired-mode-map (kbd "C-o") 'dired-create-empty-file)
(define-key dired-mode-map (kbd "b") 'dired-up-directory)

;; Open dired folders in same buffer
(put 'dired-find-alternate-file 'disabled nil)

(use-package dired-subtree
  :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

;; Activate dired-hide-details-mode by default
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
