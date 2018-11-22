(setq evil-search-module 'evil-search)
(evil-mode t)
(global-evil-visualstar-mode)
(setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))


(provide 'init-evil)
