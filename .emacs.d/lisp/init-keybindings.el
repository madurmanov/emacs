;;; init-keybindings.el -- Initialize keybindings
;;; Commentary:
;;; Code:

(windmove-default-keybindings 'super)

(global-set-key (kbd "M--") 'shrink-window-horizontally)
(global-set-key (kbd "M-=") 'enlarge-window-horizontally)
(global-set-key (kbd "M-4") 'whitespace-mode)
(global-set-key (kbd "M-5") 'indent-guide-global-mode)

(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])


(provide 'init-keybindings)
;;; init-keybindings.el ends here
