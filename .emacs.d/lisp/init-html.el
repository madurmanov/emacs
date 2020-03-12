;;; init-html.el -- Initialize php
;;; Commentary:
;;; Code:

(maybe-require-package 'web-beautify)
(maybe-require-package 'web-mode)


(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))


(provide 'init-html)
;;; init-html.el ends here
