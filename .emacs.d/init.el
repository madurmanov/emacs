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

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))


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
(show-paren-mode 1)

(setq-default
  blink-cursor-interval 0.4
  indent-tabs-mode nil
  tooltip-delay 1.5
  truncate-lines nil
  truncate-partial-width-windows nil)

(transient-mark-mode t)

(when (maybe-require-package 'indent-guide)
  (add-hook 'prog-mode-hook 'indent-guide-mode)
  (after-load 'indent-guide
    (diminish 'indent-guide-mode)))

(require-package 'cl-lib)
(require-package 'diminish)
(require-package 'scratch)
(require-package 'mwe-log-commands)
(require 'cl-lib)

(require-package 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)


;; Move line by arrows

(require-package 'move-dup)
(global-set-key [M-up] 'md/move-lines-up)
(global-set-key [M-down] 'md/move-lines-down)
(global-set-key [M-S-up] 'md/move-lines-up)
(global-set-key [M-S-down] 'md/move-lines-down)

(global-set-key (kbd "C-c d") 'md/duplicate-down)
(global-set-key (kbd "C-c D") 'md/duplicate-up)


;; Next line with reindent

(defun sanityinc/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
	 (do-left-margin (and (bolp) (> (current-left-margin) 0)))
	 (loc (point-marker))
	 ;; Don't expand an abbrev before point.
	 (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
	     (if do-left-margin (indent-to (current-left-margin)))
	     (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

    (global-set-key (kbd "C-o") 'sanityinc/open-line-with-reindent)


;; Guide Key

(require-package 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c" "C-x 4" "C-x 5" "C-c ;" "C-c ; f" "C-c ' f" "C-x n" "C-x C-r" "C-x r" "M-s" "C-h"))
(add-hook 'after-init-hook
          (lambda ()
            (guide-key-mode 1)
            (diminish 'guide-key-mode)))


;; Newline behaviour

(global-set-key (kbd "RET") 'newline-and-indent)
(defun m/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
  (global-set-key (kbd "S-<return>") 'm/newline-at-end-of-line)


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


;; Editorconfig

(require-package 'editorconfig)
(editorconfig-mode 1)


;; Whitespace

(setq-default show-trailing-whitespace t)

(require-package 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode t)

(global-set-key [remap just-one-space] 'cycle-spacing)


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

(require-package 'nlinum)
(define-globalized-minor-mode m-global-nlinum-mode nlinum-mode
  (lambda ()
    (unless (or (minibufferp)
                (derived-mode-p 'Custom-mode
                                'ibuffer-mode
                                'messages-buffer-mode
                                'help-mode))
      (nlinum-mode t))))
(setq nlinum-format "%3d \u007c ")
(m-global-nlinum-mode t)


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

(global-set-key (kbd "M-§") 'indent-guide-global-mode)
(global-set-key (kbd "M-1") 'bookmark-bmenu-list)
(global-set-key (kbd "M-2") 'bookmark-set)
(global-set-key (kbd "M-3") 'bookmark-delete)
(global-set-key (kbd "M-4") 'whitespace-mode)
(global-set-key (kbd "M-5") 'blank-mode)
(global-set-key (kbd "M-8") 'hs-minor-mode)
(global-set-key (kbd "M-9") 'nlinum-mode)
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
 '(flycheck-warning ((t (:inherit warning))))
 '(linum ((t (:background "white" :foreground "black"))))
 '(mode-line ((t (:background "black" :foreground "white"))))
 '(mode-line-inactive ((t (:background "black" :foreground "white")))))


;; Define font

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
		  (get-char-property (point) 'face))))
  (if face (message "Face: %s" face) (message "No face at %d" pos))))


;; Work as a server

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
