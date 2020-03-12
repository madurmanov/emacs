;;; init-uniquify.el -- Initialize uniquify
;;; Commentary:
;;; Code:

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " \u007c ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")


(provide 'init-uniquify)
;;; init-uniquify.el ends here
