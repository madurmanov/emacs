;;; init-css.el -- Initialize css
;;; Commentary:
;;; Code:

(maybe-require-package 'css-comb)


(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.pcss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . css-mode))


(provide 'init-css)
;;; init-css.el ends here
