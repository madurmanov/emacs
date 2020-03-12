;;; init-speedbar.el -- Initialize speedbar
;;; Commentary:
;;; Code:

(maybe-require-package 'sr-speedbar)


(setq speedbar-directory-unshown-regexp "^\\(\\.\\.*$\\)\\'")
(setq speedbar-show-unknown-files t)
(setq speedbar-use-images nil)
(setq sr-speedbar-right-side nil)
(global-set-key (kbd "M-0") 'sr-speedbar-toggle)


(provide 'init-speedbar)
;;; init-speedbar.el ends here
