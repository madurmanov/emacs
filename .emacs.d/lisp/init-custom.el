;;; init-custom.el -- Initialize custom
;;; Commentary:
;;; Code:

(custom-set-variables
 '(css-fontify-colors nil))
(custom-set-faces
 '(company-preview ((t (:background "green" :foreground "white"))))
 '(company-preview-common ((t (:background "green" :foreground "white"))))
 '(cursor ((t (:background "black" :foreground "white"))))
 '(flycheck-warning ((t (:inherit warning))))
 '(font-lock-comment-face ((t (:foreground "blue"))))
 '(header-line-highlight ((t (:background "white"))))
 '(highlight ((t (:background "white"))))
 '(isearch ((t (:background "green" :foreground "white"))))
 '(lazy-highlight ((t (:background "turquoise3" :foreground "white"))))
 '(linum ((t (:background "white" :foreground "black" :underline nil))))
 '(mode-line ((t (:background "white"))))
 '(org-todo ((t (:background "none" :foreground "red" :weight bold))))
 '(show-paren-match ((t (:background "yellow"))))
 '(speedbar-selected-face ((t (:inherit speedbar-file-face :weight bold))))
 '(web-mode-html-attr-name-face ((t (:foreground "green"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "black"))))
 '(web-mode-html-tag-face ((t (:foreground "black")))))


(provide 'init-custom)
;;; init-custom.el ends here
