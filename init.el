;; -*- lexical-binding: t; -*-
;; Emacs comes with package.el for installing packages.
;; Try M-x list-packages to see what's available.
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; setup.el provides a macro for configuration patterns
;; it makes package installation and config nice and tidy!
;; https://www.emacswiki.org/emacs/SetupEl
(if (package-installed-p 'setup)
    nil
  (if (memq 'setup package-archive-contents)
      nil
    (package-refresh-contents))
  (package-install 'setup))
(require 'setup)
(require 'use-package)

;; All other features are loaded one by one from
;; the customizations directory. Read those files
;; to find out what they do.
(add-to-list 'load-path "~/.emacs.d/customizations")

(defvar addons
  '("key-bindings.el"
    "ui.el"
    "navigation.el"
    "editing.el"
    "elisp-editing.el"
    "filetree.el"
    "git.el"
    "projects.el"
    "setup-quelpa.el"
    "setup-clojure.el"
    "setup-copilot.el"
    "setup-company.el"
    "setup-js.el"
    "setup-json.el"
    "setup-lsp.el"
    "setup-python.el"
    "setup-yaml.el"
    "shell-integration.el"
    "sql-bigquery.el"
    "dired-config.el"))

(dolist (x addons)
  (load x))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
(put 'scroll-left 'disabled nil)
