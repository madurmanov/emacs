;; This must come before configurations of
;; installed packages. Don't delete this line.
(package-initialize)


(let* ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Emacs v%s or higher is required." minver)))


(push (expand-file-name "~/.emacs.d/lisp") load-path)


(defvar best-gc-cons-threshold
  4000000
  "Best default gc threshold value. Should NOT be too big!")

;; Don't GC during startup to save time
(setq gc-cons-threshold most-positive-fixnum)


(require 'server)
(unless (server-running-p)
  (server-start))


;; Disable backup
(setq make-backup-files nil)
(setq auto-save-list-file-name nil)
(setq auto-save-default nil)


(require 'init-user)
(require 'init-packages)

(require 'init-common)
(require 'init-theme)
(require 'init-custom)

(require 'init-auto-complete)
(require 'init-bookmarks)
(require 'init-desktop)
(require 'init-editorconfig)
(require 'init-evil)
(require 'init-google-translate)
(require 'init-guide-key)
(require 'init-ibuffer)
(require 'init-ido)
(require 'init-indent-guide)
(require 'init-key-chord)
(require 'init-kakapo)
(require 'init-linum)
(require 'init-move-dup)
(require 'init-recentf)
(require 'init-smex)
(require 'init-speedbar)
(require 'init-undo-tree)
(require 'init-uniquify)
(require 'init-yasnippet)


(setq gc-cons-threshold best-gc-cons-threshold)


(provide 'init)
