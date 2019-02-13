;;; init-guide-key.el -- Initialize guide key
;;; Commentary:
;;; Code:
(require 'guide-key)

(setq guide-key/guide-key-sequence '("C-x" "C-c" "C-h"))
(add-hook 'after-init-hook
          (lambda ()
            (guide-key-mode t)
            (diminish 'guide-key-mode)))

(provide 'init-guide-key)
;;; init-guide-key.el ends here