;;; init-evil.el -- Initialize evil
;;; Commentary:
;;; Code:

(maybe-require-package 'evil)
(maybe-require-package 'evil-visualstar)


;; Use evil mode everywhere
(evil-mode t)

;; Port of the visual-start plugin for Vim to work with evil mode
(global-evil-visualstar-mode)

;; Set evil search by default search module
(setq evil-search-module 'evil-search)

;; Force evil in the certain modes
(setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))

;; Keybindings
(global-set-key (kbd "M-*") 'evil-ex-nohighlight)


(provide 'init-evil)
;;; init-evil.el ends here
