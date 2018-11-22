;;; init.el -- Initialize
;;; Commentary:
;;; Code:
(let* ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Emacs v%s or higher is required" minver)))

(push (expand-file-name "~/.emacs.d/lisp") load-path)

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

(require 'init-css)
(require 'init-js)
(require 'init-markdown)


(provide 'init)
;;; init.el ends here
