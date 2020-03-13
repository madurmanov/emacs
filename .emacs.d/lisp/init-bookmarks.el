;;; init-bookmarks.el -- Initialize bookmarks
;;; Commentary:
;;; Code:

(require 'bookmark)


(defvar m/bookmark-file-name ".emacs.bookmarks")

;; Auto save bookmarks after close
(setq bookmark-save-flag t)

;; Load bookmarks at start
(when (file-exists-p (concat "./" m/bookmark-file-name))
  (bookmark-load bookmark-default-file t))

;; Set default bookmark's file name
(setq bookmark-default-file (concat "./" m/bookmark-file-name))

;; Keybindings
(global-set-key (kbd "M-1") 'bookmark-bmenu-list)
(global-set-key (kbd "M-2") 'bookmark-set)
(global-set-key (kbd "M-3") 'bookmark-delete)


(provide 'init-bookmarks)
;;; init-bookmarks.el ends here
