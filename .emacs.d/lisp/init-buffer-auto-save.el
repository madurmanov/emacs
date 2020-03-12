;;; init-buffer-auto-save.el -- Initialize buffer auto save
;;; Commentary:
;;; Code:

(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (save-buffer)))


(add-hook 'focus-out-hook (lambda () (when buffer-file-name (save-buffer))))


(provide 'init-buffer-auto-save)
;;; init-buffer-auto-save.el ends here
