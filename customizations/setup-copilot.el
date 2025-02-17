(use-package copilot
  :ensure t
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("*.el"))
  :hook ((prog-mode . copilot-mode)
         (yaml-mode . copilot-mode)
         (markdown-mode . copilot-mode)
         (text-mode . copilot-mode))
  :custom
  (copilot-node-executable
   (concat "~/.nvm/versions/node/v22.6.0/bin/node"))
  (copilot-indent-offset-warning-disable t)
  :bind (("C-SPC" . 'copilot-complete)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("M-[" . 'copilot-previous-completion)
         ("M-]" . 'copilot-next-completion)
         ("C-<right>" . 'copilot-accept-completion-by-word)
         ("C-g" . 'copilot-clear-overlay)))

(use-package request
  :ensure t)

(use-package shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (request shell-maker)
  :custom
  (copilot-chat-frontend 'shell-maker)
  :config
  (require 'copilot-chat-shell-maker)
  (push '(shell-maker . copilot-chat-shell-maker-init) copilot-chat-frontend-list))
