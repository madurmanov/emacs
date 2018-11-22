;;; init-vcs.el -- Initialize vcs
;;; Commentary:
;;; Code:
(require 'diff-hl)

(setq diff-hl-draw-borders nil)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(global-diff-hl-mode t)
(diff-hl-flydiff-mode)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)


(provide 'init-vcs)
;;; init-vcs.el ends here