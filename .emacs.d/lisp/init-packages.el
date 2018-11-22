;;; init-packages.el -- Initialize packages
;;; Commentary:
;;; Code:
(package-initialize)

(setq package-archives
      '(("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA" . "https://melpa.org/packages/")
        ("GNU" . "http://elpa.gnu.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("MELPA" . 5)
        ("GNU" . 0))
      package-pinned-packages
      '((js2-mode . "MELPA")))

(defun require-package (package &optional min-version no-refresh)
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(require-package 'auto-yasnippet)
(require-package 'color-theme-solarized)
(require-package 'company)
(require-package 'counsel)
(require-package 'counsel-projectile)
(require-package 'css-comb)
(require-package 'diff-hl)
(require-package 'diminish)
(require-package 'editorconfig)
(require-package 'evil)
(require-package 'evil-visualstar)
(require-package 'exec-path-from-shell)
(require-package 'flycheck)
(require-package 'flx)
(require-package 'google-translate)
(require-package 'guide-key)
(require-package 'indent-guide)
(require-package 'js-doc)
(require-package 'js2-mode)
(require-package 'js2-refactor)
(require-package 'rjsx-mode)
(require-package 'key-chord)
(require-package 'magit)
(require-package 'markdown-mode)
(require-package 'move-dup)
(require-package 'page-break-lines)
(require-package 'projectile)
(require-package 'scratch)
(require-package 'smartparens)
(require-package 'smex)
(require-package 'sr-speedbar)
(require-package 'swiper)
(require-package 'undo-tree)
(require-package 'web-beautify)
(require-package 'web-mode)
(require-package 'yasnippet)


(provide 'init-packages)
;;; init-packages.el ends here
