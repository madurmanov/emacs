;;; init-ibuffer.el -- Initialize ibuffer
;;; Commentary:
;;; Code:

(require 'ibuffer)


(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode t)))
(defalias 'list-buffers 'ibuffer)


(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
