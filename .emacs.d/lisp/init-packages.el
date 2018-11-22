(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

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

(require-package 'auto-complete)
(require-package 'auto-yasnippet)
(require-package 'color-theme-solarized)
(require-package 'css-comb)
(require-package 'diminish)
(require-package 'editorconfig)
(require-package 'evil)
(require-package 'evil-visualstar)
(require-package 'google-translate)
(require-package 'guide-key)
(require-package 'indent-guide)
(require-package 'key-chord)
(require-package 'move-dup)
(require-package 'scratch)
(require-package 'smex)
(require-package 'sr-speedbar)
(require-package 'undo-tree)
(require-package 'web-beautify)
(require-package 'yasnippet)

(require 'ibuffer)


(provide 'init-packages)
