;;; init-recentf.el -- Initialize recentf
;;; Commentary:
;;; Code:
(recentf-mode t)
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude '("/tmp/"))


(provide 'init-recentf)
;;; init-recentf.el ends here