;;; init-projectile.el -- Initialize projectile
;;; Commentary:
;;; Code:
(require 'projectile)

(setq projectile-cache-file (concat user-emacs-directory "projectile.cache"))
(setq projectile-known-projects-file (concat user-emacs-directory "projectile-bookmarks.eld"))

(projectile-global-mode t)

(custom-set-variables
 '(projectile-switch-project-action (quote projectile-dired)))


(provide 'init-projectile)
;;; init-projectile.el ends here