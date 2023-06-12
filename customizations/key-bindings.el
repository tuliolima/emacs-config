; Defining my custom key bindings.

(global-set-key (kbd "C-o") 'counsel-find-file)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-<iso-lefttab>") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'undo-redo)
(global-set-key (kbd "C-p") 'ivy-switch-buffer)
(global-set-key (kbd "C-f") 'swiper)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-S-x") 'kill-region)
(global-set-key (kbd "C-S-c") 'kill-ring-save)
(global-set-key (kbd "C-S-v") 'yank)

;; Go to a symbol definition
(global-set-key (kbd "<f12>") 'lsp-find-definition)

(defun my-clojure-mode-hook () 
  (define-key clojure-mode-map (kbd "C-<return>") 'cider-eval-sexp-at-point)
  (define-key clojure-mode-map (kbd "M-RET") 'cider-eval-defun-at-point)
  (define-key clojure-mode-map (kbd "C-<right>") 'paredit-forward)
  (define-key clojure-mode-map (kbd "C-<left>") 'paredit-backward)
  (define-key clojure-mode-map (kbd "C-<down>") 'paredit-forward-down)
  (define-key clojure-mode-map (kbd "C-<up>") 'paredit-backward-up)
  (define-key clojure-mode-map (kbd "C-M-.") 'paredit-forward-slurp-sexp)
  (define-key clojure-mode-map (kbd "C-M-,") 'paredit-forward-barf-sexp))

(add-hook 'clojure-mode-hook 'my-clojure-mode-hook)

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
  '(progn (define-key paredit-mode-map (kbd "C-<right>") 'paredit-forward)
          (define-key paredit-mode-map (kbd "C-<left>") 'paredit-backward)
          (define-key paredit-mode-map (kbd "M-<right>") 'transpose-sexps)
          (define-key paredit-mode-map (kbd "M-<left>") 'reverse-transpose-sexps)))
