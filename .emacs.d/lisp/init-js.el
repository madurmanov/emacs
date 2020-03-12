;;; init-js.el -- Initialize js
;;; Commentary:
;;; Code:

(maybe-require-package 'js-doc)
(maybe-require-package 'js2-mode)
(maybe-require-package 'js2-refactor)
(maybe-require-package 'rjsx-mode)


(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . js-mode))


(provide 'init-js)
;;; init-js.el ends here
