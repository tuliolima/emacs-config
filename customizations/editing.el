;; Customizations relating to editing a buffer.

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; show line numbers
(global-display-line-numbers-mode 1)
;; but not everywhere
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; shell scripts
(setq-default sh-basic-offset 2
              sh-indentation 2)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default t)

;; comments
;; (defun toggle-comment-on-line ()
;;   "comment or uncomment current line"
;;   (interactive)
;;   (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-/") 'comment-dwim)
(global-set-key (kbd "C-?") 'comment-line)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(setq electric-indent-mode t)

;; deletes the selected region when typing
(delete-selection-mode 1)
;; deactivates auto-copy of regions after selection
(setq-default select-active-regions nil)

;; converts markdown to html
(defun markdown-html (buffer)
    (princ (with-current-buffer buffer
      (format "<!DOCTYPE html><html><xmp theme=\"united\" style=\"display:none;\">%s</xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>"
              (buffer-substring-no-properties (point-min) (point-max))))
    (current-buffer)))

(setq org-support-shift-select t)

(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; Moving a line up and down

(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun move-line-up ()
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun move-line-down ()
  (interactive)
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; Multiline editing
(use-package multiple-cursors
  :ensure t
  :demand t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ;; ("C-<" . mc/mark-previous-like-this)
   ;; ("C-S-n" . mc/unmark-previous-like-this)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/unmark-next-like-this)
   ("C-c C->" . mc/mark-all-like-this)
   ("C-c C-<" . mc/unmark-all-like-this))
  (:map mc/keymap ("<return>" . nil)))
