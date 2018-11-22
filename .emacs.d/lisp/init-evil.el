;;; init-evil.el -- Initialize evil
;;; Commentary:
;;; Code:
(require 'evil)

(evil-mode t)
(global-evil-visualstar-mode)
(setq evil-search-module 'evil-search)
(setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))


(provide 'init-evil)
;;; init-evil.el ends here
