;;; init-move-dup.el -- Initialize move dup
;;; Commentary:
;;; Code:
(global-set-key [M-up] 'md/move-lines-up)
(global-set-key [M-down] 'md/move-lines-down)
(global-set-key (kbd "C-c d") 'md/duplicate-down)
(global-set-key (kbd "C-c D") 'md/duplicate-up)

(provide 'init-move-dup)
;;; init-move-dup.el ends here