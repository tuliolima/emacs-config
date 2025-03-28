;; These customizations change the way emacs looks and disable/enable
;; some user interface elements. Some useful customizations are
;; commented out, and begin with the line "CUSTOMIZE". These are more
;; a matter of preference and may require some fiddling to match your
;; preferences

(tooltip-mode -1)                 ;; disable tooltips
(tool-bar-mode -1)                ;; the toolbar is pretty ugly
(menu-bar-mode -1)
(scroll-bar-mode -1)              ;; disable visible scrollbar
(blink-cursor-mode 0)             ;; turn off blinking cursor. distracting!
(setq create-lockfiles nil)       ;; no need for ~ files when editing
(fset 'yes-or-no-p 'y-or-n-p)     ;; changes all yes/no questions to y/n type
(setq inhibit-startup-message t)  ;; go straight to scratch buffer on startup
(setq ring-bell-function 'ignore) ;; turn off audible bell

;; show a vertical line to help limiting line width
(setq-default display-fill-column-indicator-column 80)
(global-display-fill-column-indicator-mode 1)

;; show full path in title bar
(setq-default frame-title-format "%b (%f)")

;; initial frame height and width
(add-to-list 'default-frame-alist '(height . 80))
(add-to-list 'default-frame-alist '(width . 100))

;; set the default font size
(set-face-attribute 'default nil :height 130)

;; zoom in/out with C-+ and C--
(defun zoom-in ()
  "Increase the font size."
  (interactive)
  (set-face-attribute 'default nil :height
                      (+ (face-attribute 'default :height) 10)))

(defun zoom-out ()
  "Decrease the font size."
  (interactive)
  (set-face-attribute 'default nil :height
                      (- (face-attribute 'default :height) 10)))

(global-set-key (kbd "C-+") 'zoom-in)
(global-set-key (kbd "C-=") 'zoom-in)
(global-set-key (kbd "C--") 'zoom-out)

;; on a Mac, don't pop up font menu
(when (string-equal system-type "darwin")
  'ok
  (global-set-key (kbd "s-t") #'(lambda () (interactive))))

(use-package nerd-icons
  :ensure t)

;; Needed to show Github notifications in the modeline
(use-package ghub
  :ensure t)

;; doom is a whole Emacs distribution unto itself,
;; but it's got some really nice packages that you
;; can use a-la-carte. doom-modeline is simply a more
;; modern and more beautiful modeline.
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom (doom-modeline-github t))

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      x-select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; CUSTOMIZE

;; Your choice of font is very personal, and you must have installed it
;; on your system before you specify it here,
;; Some font suggestions:  https://www.creativebloq.com/features/the-best-monospace-fonts-for-coding
;; (set-face-attribute 'default nil :font "Fira Code")

;; Closes the current window
(global-set-key (kbd "C-w") 'delete-window)

;; Highlight some key words
(font-lock-add-keywords nil '(("\\b\\(FIXME\\|TODO\\|BUG\\)\\b" 1 font-lock-warning-face t)))

(add-hook 'emacs-startup-hook #'projectile-dired)
