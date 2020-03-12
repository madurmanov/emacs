;;; init-evil.el -- Initialize evil
;;; Commentary:
;;; Code:

(maybe-require-package 'evil)
(maybe-require-package 'evil-visualstar)


(evil-mode t)
(global-evil-visualstar-mode)
(setq evil-search-module 'evil-search)
(setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))

(global-set-key (kbd "M-*") 'evil-ex-nohighlight)


(provide 'init-evil)
;;; init-evil.el ends here
