;;; init-desktop.el -- Initialize desktop
;;; Commentary:
;;; Code:

(require 'desktop)


(setq desktop-dirname "./"
      desktop-path (list desktop-dirname)
      desktop-load-locked-desktop nil)
(desktop-save-mode t)


(provide 'init-desktop)
;;; init-desktop.el ends here
