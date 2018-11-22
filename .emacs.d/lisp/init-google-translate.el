(setq google-translate-translation-directions-alist
  '(("en" . "ru") ("ru" . "en")))
(global-set-key (kbd "C-c g t") 'google-translate-at-point)
(global-set-key (kbd "C-c g T") 'google-translate-query-translate)
(global-set-key (kbd "C-c g g") 'google-translate-smooth-translate)


(provide 'init-google-translate)
