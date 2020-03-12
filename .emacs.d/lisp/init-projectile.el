;;; init-projectile.el -- Initialize projectile
;;; Commentary:
;;; Code:

(maybe-require-package 'projectile)
(maybe-require-package 'ibuffer-projectile)


(add-hook 'after-init-hook 'projectile-mode)

;; Shorter modeline
(setq-default projectile-mode-line-prefix " Pr")

(after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


(provide 'init-projectile)
;;; init-projectile.el ends here
