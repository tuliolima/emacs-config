(define-key dired-mode-map (kbd "C-o") 'dired-create-empty-file)

;; Open dired folders in same buffer
(put 'dired-find-alternate-file 'disabled nil)
