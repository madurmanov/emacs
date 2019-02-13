;;; init-system.el -- Initialize system
;;; Commentary:
;;; Code:
(require 'flycheck)
(require 'paren)
(require 'smartparens-config)
(require 'whitespace)
(require 'windmove)

(load-theme 'solarized t)

(setq user-full-name "Mikhail Durmanov")
(setq user-mail-address "madurmanov@gmail.com")

(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

;; Disable backups
(setq make-backup-files nil)
(setq auto-save-list-file-name nil)
(setq auto-save-default nil)

(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize)
  (menu-bar-mode +1))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq
  custom-file (expand-file-name "custom.el" user-emacs-directory)
  blink-matching-paren nil
  flycheck-indication-mode 'right-fringe
  inhibit-startup-echo-area-message t
  use-file-dialog nil
  use-dialog-box nil
  scroll-margin 0
  scroll-conservatively 100000
  scroll-preserve-screen-position 1
  show-paren-style 'parenthesis
  whitespace-line-column 80
  require-final-newline t)

(setq-default
  blink-cursor-interval 0.4
  tooltip-delay 1.5
  show-trailing-whitespace t
  initial-scratch-message (concat ";; Happy hacking, " user-login-name "\n\n"))

(editorconfig-mode t)
(delete-selection-mode t)
(global-hl-line-mode)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(show-paren-mode +1)
(transient-mark-mode t)
(menu-bar-mode -1)
(smartparens-global-mode 1)
(global-auto-revert-mode t)
(global-page-break-lines-mode)
(global-undo-tree-mode 1)

(windmove-default-keybindings 'super)

(global-set-key (kbd "M--") 'shrink-window-horizontally)
(global-set-key (kbd "M-=") 'enlarge-window-horizontally)
(global-set-key (kbd "M-4") 'whitespace-mode)
(global-set-key (kbd "M-5") 'indent-guide-global-mode)

(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])

(add-hook 'prog-mode-hook 'flycheck-mode)

(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (save-buffer)))

(add-hook 'focus-out-hook (lambda () (when buffer-file-name (save-buffer))))

(custom-set-variables
 '(css-fontify-colors nil))
(custom-set-faces
 '(flycheck-warning ((t (:inherit warning))))
 '(linum ((t (:background "white" :foreground "black" :strike-through nil :overline nil :underline nil :weight normal))))
 '(mode-line ((t (:background "black" :foreground "white"))))
 '(mode-line-inactive ((t (:background "black" :foreground "white"))))
 '(org-todo ((t (:background "none" :foreground "red" :weight bold)))))

(provide 'init-system)
;;; init-system.el ends here