(add-hook 'after-change-major-mode-hook 'kakapo-mode)
(define-key evil-normal-state-map "o" (lambda () (interactive) (kakapo-open nil)))
(define-key evil-normal-state-map "O" (lambda () (interactive) (kakapo-open t)))
(define-key evil-insert-state-map (kbd "RET") 'kakapo-ret-and-indent)
(define-key evil-insert-state-map (kbd "DEL") 'kakapo-backspace)
(define-key evil-insert-state-map (kbd "<S-backspace>") 'kakapo-upline)


(provide 'init-kakapo)
