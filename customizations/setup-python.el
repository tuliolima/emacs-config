;; (setup (:package elpy)
;;   (:hook python-mode))

;; (setup (:package flycheck))

;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

;; (setup (:package blacken))

;; !!! You must `run pip install 'python-lsp-server[all]'`

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))
