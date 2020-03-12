;;; init-flycheck.el -- Initialize flycheck
;;; Commentary:
;;; Code:

(maybe-require-package 'flycheck)


(setq flycheck-indication-mode 'right-fringe)

(add-hook 'prog-mode-hook 'flycheck-mode)


(provide 'init-flycheck)
;;; init-flycheck.el ends here
