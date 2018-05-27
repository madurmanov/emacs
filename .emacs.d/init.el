(setq user-full-name "Mikhail Durmanov")
(setq user-mail-address "madurmanov@gmail.com")

(require 'server)
(unless (server-running-p)
  (server-start))


;; Disable backup

(setq make-backup-files nil)
(setq auto-save-list-file-name nil)
(setq auto-save-default nil)


;; Save desktop

(setq desktop-dirname "./"
      desktop-path (list desktop-dirname)
      desktop-load-locked-desktop nil)
(desktop-save-mode t)


;; Bookmarks

(setq bookmark-save-flag t)
(when (file-exists-p "./.emacs.bookmarks")
  (bookmark-load bookmark-default-file t))
(setq bookmark-default-file "./.emacs.bookmarks")

(global-set-key (kbd "M-1") 'bookmark-bmenu-list)
(global-set-key (kbd "M-2") 'bookmark-set)
(global-set-key (kbd "M-3") 'bookmark-delete)


;; Common

(defalias 'yes-or-no-p 'y-or-n-p)

(setq
  custom-file (expand-file-name "custom.el" user-emacs-directory)
  inhibit-startup-echo-area-message t
  use-file-dialog nil
  use-dialog-box nil)

(setq-default
  blink-cursor-interval 0.4
  tooltip-delay 1.5
  initial-scratch-message (concat ";; Happy hacking, " user-login-name "\n\n"))

(delete-selection-mode t)
(global-hl-line-mode)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(show-paren-mode t)
(transient-mark-mode t)
(electric-indent-mode -1)
(electric-pair-mode t)


;; Packages

(package-initialize)
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

(require-package 'diminish)
(require-package 'scratch)


;; Editorconfig

(require-package 'editorconfig)
(editorconfig-mode t)
(diminish 'editorconfig-mode)


;; Theme

(require-package 'color-theme-solarized)
(load-theme 'solarized t)
(custom-set-faces
 '(flycheck-warning ((t (:inherit warning))))
 '(linum ((t (:background "white" :foreground "black"))))
 '(mode-line ((t (:background "black" :foreground "white"))))
 '(mode-line-inactive ((t (:background "black" :foreground "white"))))
 '(org-todo ((t (:background "none" :foreground "red" :weight bold)))))


;; Resize buffer

(global-set-key (kbd "M--") 'shrink-window-horizontally)
(global-set-key (kbd "M-=") 'enlarge-window-horizontally)


;; Redefine meta key from ESC

(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])


;; Move dup

(require-package 'move-dup)
(global-set-key [M-up] 'md/move-lines-up)
(global-set-key [M-down] 'md/move-lines-down)
(global-set-key (kbd "C-c d") 'md/duplicate-down)
(global-set-key (kbd "C-c D") 'md/duplicate-up)


;; Guide key

(require-package 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c" "C-h"))
(add-hook 'after-init-hook
          (lambda ()
            (guide-key-mode t)
            (diminish 'guide-key-mode)))


;; Undo tree

(require-package 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)


;; Smex

(require-package 'smex)
(setq-default smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(global-set-key [remap execute-extended-command] 'smex)


;; Ido

(ido-mode t)
(icomplete-mode t)
(ido-everywhere t)
(setq ido-virtual-buffers t)
(setq ido-enable-flex-matching t)


;; Recentf

(recentf-mode t)
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude '("/tmp/"))


;; Whitespace

(setq-default show-trailing-whitespace t)
(global-set-key (kbd "M-4") 'whitespace-mode)


;; Evil

(require-package 'evil)
(require-package 'evil-visualstar)
(setq evil-search-module 'evil-search)
(evil-mode t)
(global-evil-visualstar-mode)
(setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))


;; Key chord

(require-package 'key-chord)
(key-chord-mode t)
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)


;; Kakapo

(require-package 'kakapo-mode)
(add-hook 'after-change-major-mode-hook 'kakapo-mode)
(define-key evil-normal-state-map "o" (lambda () (interactive) (kakapo-open nil)))
(define-key evil-normal-state-map "O" (lambda () (interactive) (kakapo-open t)))
(define-key evil-insert-state-map (kbd "RET") 'kakapo-ret-and-indent)
(define-key evil-insert-state-map (kbd "DEL") 'kakapo-backspace)
(define-key evil-insert-state-map (kbd "<S-backspace>") 'kakapo-upline)


;; Linum

(setq linum-format "%3d \u007c ")
(global-linum-mode t)
(global-set-key (kbd "M-9") 'linum-mode)


;; Ibuffer

(require 'ibuffer)
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode t)))
(defalias 'list-buffers 'ibuffer)


;; Uniquify buffers names

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " \u007c ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")


;; Yasnippet

(require-package 'yasnippet)
(yas-global-mode t)
(setq yas-snippet-dirs
      '(concat user-emacs-directory "snippets"))
(diminish 'yas-minor-mode)


;; Auto complete

(require-package 'auto-complete)
(ac-config-default)
(setq ac-auto-show-menu .5)
(setq ac-delay .1)
(setq ac-auto-start t)
(setq self-insert-command 2)
(diminish 'auto-complete-mode)


;; Speedbar

(require-package 'sr-speedbar)
(setq speedbar-directory-unshown-regexp "^\\(\\.\\.*$\\)\\'")
(setq speedbar-show-unknown-files t)
(setq speedbar-use-images nil)
(setq sr-speedbar-right-side nil)
(global-set-key (kbd "M-0") 'sr-speedbar-toggle)


;; Google translate

(require-package 'google-translate)
(setq google-translate-translation-directions-alist
  '(("en" . "ru") ("ru" . "en")))
(global-set-key (kbd "C-c g t") 'google-translate-at-point)
(global-set-key (kbd "C-c g T") 'google-translate-query-translate)
(global-set-key (kbd "C-c g g") 'google-translate-smooth-translate)


(provide 'init)
