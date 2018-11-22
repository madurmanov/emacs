(setq bookmark-save-flag t)
(when (file-exists-p "./.emacs.bookmarks")
  (bookmark-load bookmark-default-file t))
(setq bookmark-default-file "./.emacs.bookmarks")

(global-set-key (kbd "M-1") 'bookmark-bmenu-list)
(global-set-key (kbd "M-2") 'bookmark-set)
(global-set-key (kbd "M-3") 'bookmark-delete)


(provide 'init-bookmarks)
