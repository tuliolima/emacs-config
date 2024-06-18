;; See:  https://clojure-lsp.io/
;; also: https://emacs-lsp.github.io/lsp-mode/
(setup (:package lsp-mode lsp-ui lsp-ivy lsp-treemacs)
  (:hook lsp-enable-which-key-integration)
  (:bind "M-<f7>" lsp-find-references))

;; (use-package flycheck-clj-kondo
;;   :ensure t)

;; clojure-mode is (naturally) the major mode for editing
;; Clojure and ClojureScript. subword-mode allows words
;; in camel case to be treated as separate words for
;; movement and editing commands.
;; https://github.com/clojure-emacs/clojure-mode
;; subword-mode is useful for working with camel-case tokens,
;; like names of Java classes (e.g. JavaClassName)
(setup (:package clojure-mode)
  (:hook subword-mode
         lsp))

(use-package paredit
  :ensure t
  :hook
  (clojure-mode . enable-paredit-mode)
  (emacs-lisp-mode . enable-paredit-mode)
  (cider-repl-mode . enable-paredit-mode)
  (lisp-mode . enable-paredit-mode)
  :bind
  (:map paredit-mode-map
   ("C-<right>" . paredit-forward)
   ("C-<left>" . paredit-backward)
   ("C-<up>" . paredit-backward-up)
   ("C-<down>" . paredit-forward-down)
   ("C-M-." . paredit-forward-slurp-sexp)
   ("C-M-," . paredit-forward-barf-sexp)))

(use-package cider
  :ensure t
  :bind (:map cider-mode-map
         ("C-c u" . cider-user-ns)
         ("C-M-r" . cider-refresh)
         ("C-<return>" . cider-eval-sexp-at-point)
         ("M-RET" . cider-eval-defun-at-point))
  :custom
  (cider-show-error-buffer t)
  (cider-auto-select-error-buffer t)
  (cider-repl-history-file "~/.emacs.d/cider-history")
  (cider-repl-pop-to-buffer-on-connect t)
  (cider-repl-wrap-history t)
  (cider-test-show-report-on-success t))

;; hydra provides a nice looking menu for commands
;; to see what's available, use M-x and the prefix cider-hydra
;; https://github.com/clojure-emacs/cider-hydra
(setup (:package cider-hydra)
  (:hook-into clojure-mode))

;; additional refactorings for CIDER
;; e.g. add missing libspec, extract function, destructure keys
;; https://github.com/clojure-emacs/clj-refactor.el
(setup (:package clj-refactor)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (:hook-into clojure-mode))

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

;; these help me out with the way I usually develop web apps
(defun cider-start-http-server ()
  (interactive)
  (cider-load-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))

(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

(setq clojure-toplevel-inside-comment-form t)
(setq cider-eval-spinner-type 'moon)
(setq cider-overlays-use-font-lock t)
(setq cider-result-overlay-position 'at-point)
(setq cider-eval-result-duration 'change)
(setq cider-save-file-on-load t)
(setq cider-use-overlays t)
