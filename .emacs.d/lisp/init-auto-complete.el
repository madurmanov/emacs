;;; init-auto-complete.el -- Initialize auto complete
;;; Commentary:
;;; Code:

(maybe-require-package 'company)
(maybe-require-package 'counsel)
(maybe-require-package 'counsel-projectile)
(maybe-require-package 'flx)
(maybe-require-package 'smex)
(maybe-require-package 'swiper)
(maybe-require-package 'ivy)
(maybe-require-package 'projectile)


(ivy-mode 1)
(setq projectile-completion-system 'ivy)
(counsel-projectile-mode)

(setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t . ivy--regex-fuzzy)))

(setq company-idle-delay 0.5)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-flip-when-above t)
(global-company-mode 1)

(setq-default smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(global-set-key [remap execute-extended-command] 'smex)


(provide 'init-auto-complete)
;;; init-auto-complete.el ends here
