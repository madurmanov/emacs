;;; init-desktop.el -- Initialize desktop
;;; Commentary:
;;; Code:

(require 'desktop)


;; Save desktop after exit
(desktop-save-mode t)

;; Save to Emacs start directory
(setq desktop-dirname "./")

;; Load desktops saved before
(setq desktop-path (list desktop-dirname))

;; Disable to load locked desktops
(setq desktop-load-locked-desktop nil)


(provide 'init-desktop)
;;; init-desktop.el ends here
