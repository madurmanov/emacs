;;; init-yasnippet.el -- Initialize yasippet
;;; Commentary:
;;; Code:
(require 'yasnippet)

(add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))

(yas-global-mode t)
(setq yas-snippet-dirs
      '(concat user-emacs-directory "snippets"))
(setq-default mode-require-final-newline nil)

(setq yas-prompt-functions '(yas-dropdown-prompt
			     yas-ido-prompt
			     yas-completing-prompt))

(defadvice yas-insert-snippet (around use-completing-prompt activate)
  "Use `yas-completing-prompt' for `yas-prompt-functions' but only here..."
  (let* ((yas-prompt-functions '(yas-completing-prompt)))
    ad-do-it))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here