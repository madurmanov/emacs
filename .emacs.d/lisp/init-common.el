(defalias 'yes-or-no-p 'y-or-n-p)

(setq
  custom-file (expand-file-name "custom.el" user-emacs-directory)
  inhibit-startup-echo-area-message t
  use-file-dialog nil
  use-dialog-box nil)

(setq-default
  blink-cursor-interval 0.4
  tooltip-delay 1.5
  initial-scratch-message (concat ";; Happy hacking, " user-login-name "\n\n"))

(delete-selection-mode t)
(global-hl-line-mode)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(show-paren-mode t)
(transient-mark-mode t)
(electric-indent-mode -1)
(electric-pair-mode t)
(menu-bar-mode -1)


;; Resize buffer
(global-set-key (kbd "M--") 'shrink-window-horizontally)
(global-set-key (kbd "M-=") 'enlarge-window-horizontally)


;; Redefine meta key from ESC
(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])


;; Whitespace
(setq-default show-trailing-whitespace t)
(global-set-key (kbd "M-4") 'whitespace-mode)


(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . js-jsx-mode))


(provide 'init-common)
