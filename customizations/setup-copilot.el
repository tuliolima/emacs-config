(use-package copilot
  :ensure t
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("*.el"))
  :hook (prog-mode . copilot-mode)
  :custom
  (copilot-node-executable "/home/tulio.abner/.nvm/versions/node/v20.13.1/bin/node")
  (copilot-indent-offset-warning-disable t)
  :bind (("C-SPC" . 'copilot-complete)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("M-[" . 'copilot-previous-completion)
         ("M-]" . 'copilot-next-completion)
         ("C-<right>" . 'copilot-accept-completion-by-word)
         ("C-g" . 'copilot-clear-overlay)))
