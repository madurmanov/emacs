;;; init-linum.el -- Initialize linum
;;; Commentary:
;;; Code:
(require 'linum)

(setq linum-format "%3d \u007c ")
(global-linum-mode t)
(global-set-key (kbd "M-9") 'linum-mode)
(setq linum-disabled-modes-list '(compilation-mode
                                  fundamental-mode
                                  help-mode
                                  ibuffer-mode
                                  Info-mode
                                  org-mode
                                  speedbar-mode
                                  text-mode
                                  woman-mode))
(defun linum-on () (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)) (linum-mode 1)))


(provide 'init-linum)
;;; init-linum.el ends here