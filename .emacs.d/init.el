;;; init.el --- Initialize
;;; Commentary:
;;; Code:

;; Adjust garbage collector thresholds for decrease startup time
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 20 1024 1024))))

(let* ((minver "26.3"))
  (when (version< emacs-version minver)
    (error "Emacs v%s or higher is required" minver)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Resolve flycheck load local lisp files error
(setq flycheck-emacs-lisp-load-path 'inherit)

(require 'init-packages)
(require 'init-system)

(require 'init-auto-complete)
(require 'init-bookmarks)
(require 'init-desktop)
(require 'init-evil)
(require 'init-google-translate)
(require 'init-guide-key)
(require 'init-ibuffer)
(require 'init-key-chord)
(require 'init-linum)
(require 'init-move-dup)
(require 'init-projectile)
(require 'init-recentf)
(require 'init-speedbar)
(require 'init-uniquify)
(require 'init-vcs)
(require 'init-yasnippet)

(require 'init-diminish)

(require 'init-html)
(require 'init-css)
(require 'init-js)
(require 'init-php)
(require 'init-markdown)

(provide 'init)
;;; init.el ends here
