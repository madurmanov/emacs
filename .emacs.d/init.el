(setq user-full-name "Mikhail Durmanov")
(setq user-mail-address "madurmanov@gmail.com")


;; Temporarily reduce garbage collection during startup

(defconst m/initial-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold m/initial-gc-cons-threshold)))


;; Disable backup

(setq make-backup-files nil)
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)
(setq auto-save-default nil)


;; Packages

(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq package-list '(auto-complete
                     blank-mode
                     evil
                     evil-visualstar
                     google-translate
                     highlight-parentheses
                     indent-guide
                     jade-mode
                     kakapo-mode
                     key-chord
                     php-mode
                     projectile
                     scss-mode
                     sr-speedbar
                     web-mode
                     yasnippet))

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

(defun maybe-require-package (package &optional min-version no-refresh)
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

(dolist (package package-list)
  (require-package package))


;; Associate files extension with modes

(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.pcss\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))


;; Interface

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(defalias 'yes-or-no-p 'y-or-n-p)
(setq ingibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq redisplay-dont-pause t)
(setq indicate-empty-lines t)
(setq ring-bell-function 'ignore)
(delete-selection-mode t)
(global-hl-line-mode)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(require-package 'cl-lib)
(require-package 'diminish)
(require-package 'scratch)
(require-package 'mwe-log-commands)
(require 'cl-lib)


;; Smex

(require-package 'smex)
(setq-default smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(global-set-key [remap execute-extended-command] 'smex)


;; Save desktop

(setq desktop-dirname "./"
      desktop-path (list desktop-dirname)
      desktop-load-locked-desktop nil)
(desktop-save-mode 1)


;; Bookmarks

(setq bookmark-save-flag t)
(when (file-exists-p "./.emacs.bookmarks")
  (bookmark-load bookmark-default-file t))
(setq bookmark-default-file "./.emacs.bookmarks")


;; Delete trailing whitespaces before save

(add-to-list 'write-file-functions 'delete-trailing-whitespace)


;; Electric mode

(electric-pair-mode 1)
(electric-indent-mode -1)


;; Editorconfig

(require-package 'editorconfig)
(editorconfig-mode 1)


;; Hightlight parentheses

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)
(show-paren-mode t)


;; Whitespace

(setq whitespace-display-mappings
      '((space-mark ?\ [?\xB7] [?.])))


;; Evil

(setq evil-search-module 'evil-search)
(evil-mode 1)
(global-evil-visualstar-mode)
(setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))


;; Kakapo

(setq kakapo-list '(fundamental-mode-hook
                    text-mode-hook
                    lisp-interaction-mode-hook
                    lisp-mode-hook
                    emacs-list-mode-hook
                    html-mode-hook
                    jade-mode-hook
                    css-mode-hook
                    scss-mode-hook
                    js-mode-hook
                    php-mode-hook
                    perl-mode-hook))
(dolist (hook kakapo-list)
  (add-hook hook 'kakapo-mode))
(define-key evil-normal-state-map "o" (lambda () (interactive) (kakapo-open nil)))
(define-key evil-normal-state-map "O" (lambda () (interactive) (kakapo-open t)))
(define-key evil-insert-state-map (kbd "RET") 'kakapo-ret-and-indent)
(define-key evil-insert-state-map (kbd "DEL") 'kakapo-backspace)
(define-key evil-insert-state-map (kbd "<S-backspace>") 'kakapo-upline)


;; Linum

(define-globalized-minor-mode m-global-linum-mode linum-mode
  (lambda ()
    (unless (or (minibufferp)
                (derived-mode-p 'Custom-mode
                                'ibuffer-mode
                                'messages-buffer-mode
                                'help-mode))
      (linum-mode t))))
(setq linum-format "%3d \u007c ")
(m-global-linum-mode t)


;; Uniquify

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " \u007c ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")


;; Ido

(ido-mode t)
(icomplete-mode t)
(ido-everywhere t)
(setq ido-virtual-buffers t)
(setq ido-enable-flex-matching t)


;; Ibuffer

(require 'ibuffer)

(setq ibuffer-display-summary nil)
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode t)))
(defalias 'list-buffers 'ibuffer)
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))


;; Yasnippet

(yas-global-mode 1)
(setq yas-snippet-dirs
      '(concat user-emacs-directory "snippets"))


;; Auto complete

(ac-config-default)
(setq ac-auto-show-menu .5)
(setq ac-delay .1)
(setq ac-auto-start t)
(setq self-insert-command 2)


;; Speedbar

(setq speedbar-directory-unshown-regexp "^\\(\\.\\.*$\\)\\'")


;; Bookmark settings

(setq bookmark-save-flag t)
(when (file-exists-p "./.emacs.bookmarks")
  (bookmark-load bookmark-default-file t))
(setq bookmark-default-file "./.emacs.bookmarks")


;; Key-chord

(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)


;; Recentf

(recentf-mode 1)
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude '("/tmp/"))


;; Flycheck

(require-package 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)
(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)


;; Mmm

(require-package 'mmm-mode)
(require 'mmm-auto)

(setq mmm-global-mode 'buffers-with-submode-classes)
(setq mmm-submode-decoration-level 2)


;; Google translate

(setq google-translate-translation-directions-alist
  '(("en" . "ru") ("ru" . "en")))


;; Shortcuts

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-§") 'indent-guide-global-mode)
(global-set-key (kbd "M-1") 'bookmark-bmenu-list)
(global-set-key (kbd "M-2") 'bookmark-set)
(global-set-key (kbd "M-3") 'bookmark-delete)
(global-set-key (kbd "M-4") 'whitespace-mode)
(global-set-key (kbd "M-5") 'blank-mode)
(global-set-key (kbd "M-8") 'hs-minor-mode)
(global-set-key (kbd "M-9") 'linum-mode)
(global-set-key (kbd "M-0") 'sr-speedbar-toggle)
(global-set-key (kbd "M-[") 'hs-toggle-hiding)
(global-set-key (kbd "M-]") 'hs-show-all)
(global-set-key (kbd "C-c g t") 'google-translate-at-point)
(global-set-key (kbd "C-c g T") 'google-translate-query-translate)
(global-set-key (kbd "C-c g g") 'google-translate-smooth-translate)


;; Theme

(require-package 'color-theme-solarized)
(load-theme 'solarized t)
(custom-set-faces
 '(linum ((t (:background "white" :foreground "black"))))
 '(mode-line ((t (:background "black" :foreground "white"))))
 '(mode-line-inactive ((t (:background "black" :foreground "white")))))


;; Work as a server

(require 'server)
(unless (server-running-p)
  (server-start))
