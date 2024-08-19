(define-key dired-mode-map (kbd "C-o") 'dired-create-empty-file)
(define-key dired-mode-map (kbd "b") 'dired-up-directory)

;; Open dired folders in same buffer
(put 'dired-find-alternate-file 'disabled nil)
