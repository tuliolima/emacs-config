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

(defun my-dired-show-readme ()
  "Show the content of README.md in the dired buffer if it exists."
  (when (and (eq major-mode 'dired-mode)
             (not (file-remote-p default-directory))) ;; Avoid remote directories
    (let* ((files (directory-files default-directory))
           (readme-file (seq-find (lambda (f)
                                    (string-match-p "\\`readme\\.md\\'" (downcase f)))
                                  files)))
      (when readme-file
        (let ((inhibit-read-only t)) ;; Temporarily allow buffer modification
          (save-excursion
            (goto-char (point-max))
            (insert "\n\n")
            (insert-file-contents readme-file)
            (message "Inserted README.md at the end of the buffer.")))))))

(add-hook 'dired-after-readin-hook 'my-dired-show-readme)
