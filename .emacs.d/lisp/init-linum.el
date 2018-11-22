(setq linum-format "%3d \u007c ")
(global-linum-mode t)
(global-set-key (kbd "M-9") 'linum-mode)
(setq linum-disabled-modes-list '(help-mode ibuffer-mode compilation-mode))
(defun linum-on () (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)) (linum-mode 1)))


(provide 'init-linum)
