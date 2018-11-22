(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode t)))
(defalias 'list-buffers 'ibuffer)


(provide 'init-ibuffer)
