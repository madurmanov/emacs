;;; init-speedbar.el -- Initialize speedbar
;;; Commentary:
;;; Code:

(maybe-require-package 'sr-speedbar)


;; Regular expression matching directories not to show in the speedbar
(setq speedbar-directory-unshown-regexp "^\\(\\.\\.*$\\)\\'")

;; Show files of all types
(setq speedbar-show-unknown-files t)

;; Disable use images because CLI
(setq speedbar-use-images nil)

;; Trim large directories names
(setq speedbar-directory-button-trim-method 'trim)

;; Set fixed width size
(setq sr-speedbar-width 40)
(setq sr-speedbar-max-width 40)

;; Expanded sub-nodes indention size
(setq speedbar-indentation-width 2)

;; Show speedbar at the left side
(setq sr-speedbar-right-side nil)

;; Keybindings
(global-set-key (kbd "M-)") 'speedbar-refresh)
(global-set-key (kbd "M-0") 'sr-speedbar-toggle)


(provide 'init-speedbar)
;;; init-speedbar.el ends here
