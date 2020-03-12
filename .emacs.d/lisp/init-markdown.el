;;; init-markdown.el -- Initialize markdown
;;; Commentary:
;;; Code:

(maybe-require-package 'markdown-mode)


(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'markdown-mode-hook 'turn-on-auto-fill)


(provide 'init-markdown)
;;; init-markdown.el ends here
