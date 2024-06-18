; Defining my custom key bindings.

(global-set-key (kbd "C-o") 'counsel-find-file)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-<iso-lefttab>") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'undo-redo)
(global-set-key (kbd "s-<tab>") 'ivy-switch-buffer)
(global-set-key (kbd "C-f") 'swiper)
(global-set-key (kbd "C-s") 'save-buffer)

;; Enable CUA key bindings
(cua-mode t)

;; Go to a symbol definition
(global-set-key (kbd "<f12>") 'lsp-find-definition)

(defun reverse-transpose-sexps (arg)
  (interactive "*p")
  (transpose-sexps (- arg))
  ;; when transpose-sexps can no longer transpose, it throws an error and code
  ;; below this line won't be executed. So, we don't have to worry about side
  ;; effects of backward-sexp and forward-sexp.
  ;; (backward-sexp (1+ arg))
  ;; (forward-sexp 1)
)

(eval-after-load 'paredit
  '(progn (define-key paredit-mode-map (kbd "M-<right>") 'transpose-sexps)
          (define-key paredit-mode-map (kbd "M-<left>") 'reverse-transpose-sexps)))
