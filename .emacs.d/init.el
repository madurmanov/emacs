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


;; Resolve flycheck's load local lisp files error
(setq flycheck-emacs-lisp-load-path 'inherit)


;; Core
(require 'init-packages)
(require 'init-core)
(require 'init-utils)
(require 'init-custom)
(require 'init-keybindings)

;; Packages
(require 'init-auto-complete)
(require 'init-bookmarks)
(require 'init-buffer-auto-save)
(require 'init-desktop)
(require 'init-diminish)
(require 'init-evil)
(require 'init-flycheck)
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

;; Languages
(require 'init-css)
(require 'init-html)
(require 'init-js)
(require 'init-markdown)
(require 'init-php)


(provide 'init)
;;; init.el ends here
