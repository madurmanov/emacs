;;; init-core.el -- Initialize core
;;; Commentary:
;;; Code:

(require 'paren)
(require 'whitespace)
(require 'windmove)
(maybe-require-package 'smartparens)
(maybe-require-package 'editorconfig)
(maybe-require-package 'indent-guide)
(maybe-require-package 'undo-tree)
(maybe-require-package 'scratch)
(maybe-require-package 'page-break-lines)
(maybe-require-package 'exec-path-from-shell)


(setq user-full-name "Mikhail Durmanov")
(setq user-mail-address "madurmanov@gmail.com")


;; Disable backups
(setq make-backup-files nil)
(setq auto-save-list-file-name nil)
(setq auto-save-default nil)

;; Use environment variables from the user's shell when system is MacOS
(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize)
  (menu-bar-mode +1))

;; Short answers
(defalias 'yes-or-no-p 'y-or-n-p)


;; Set variables
(setq
  custom-file (expand-file-name "custom.el" user-emacs-directory)
  blink-matching-paren nil
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


;; Set modes
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


(provide 'init-core)
;;; init-core.el ends here
