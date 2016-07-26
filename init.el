(set-language-environment 'UTF-8)
(setq user-full-name "Mikhail Durmanov")
(setq user-mail-adress "madurmanov@gmail.com")

;; Disable backup and auto save files
(setq make-backup-files nil)
(setq auto-save-list-file-name nil)
(setq auto-save-default nil)

;; Save open buffers
(setq desktop-dirname "./"
      desktop-path (list desktop-dirname)
      desktop-load-locked-desktop nil)
(desktop-save-mode 1)

;; Work as a server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Packages
(setq package-archives '(("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(setq package-list '(auto-complete
                     blank-mode
                     editorconfig
                     evil
                     highlight-parentheses
                     indent-guide
                     jade-mode
                     kakapo-mode
                     key-chord
                     php-mode
                     projectile
                     rainbow-delimiters
                     scss-mode
                     smex
                     sr-speedbar
                     web-mode
                     yasnippet))
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
(add-to-list 'load-path (concat user-emacs-directory "elpa/"))

;; Associate files extension
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.pcss\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))

;; Delete trailing whitespaces before save
(add-to-list 'write-file-functions 'delete-trailing-whitespace)

;; Disable start screen
(setq inhibit-splash-screen t)
(setq ingibit-startup-message t)

;; Disable GUI components
(menu-bar-mode -1)
(setq use-dialog-box nil)
(setq redisplay-dont-pause t)
(setq ring-bell-function 'ignore)

;; Highlight syntax
(global-font-lock-mode 1)

;; Show file size in mode line
(size-indication-mode t)

;; Short messages
(defalias 'yes-or-no-p 'y-or-n-p)

;; Electric mode
(electric-pair-mode 1)
(electric-indent-mode -1)

;; Delete selection
(delete-selection-mode t)

;; Editorconfig
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
(global-set-key (kbd "M-6") 'whitespace-mode)
(global-set-key (kbd "M-7") 'blank-mode)

;; Evil
(evil-mode 1)
(evil-set-initial-state 'bs-mode 'emacs)

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

;; Folding
(defvar hs-special-modes-alist
  (mapcar 'purecopy
          '((css-mode "{" "}" nil nil))))
(add-hook 'css-mode-hook (lambda () (hs-minor-mode 1)))
(global-set-key (kbd "M-8") 'hs-minor-mode)
(global-set-key (kbd "M-[") 'hs-toggle-hiding)
(global-set-key (kbd "M-]") 'hs-show-all)

;; Linum
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
         (count-lines (point-min) (point-max)))))
   (linum-format (concat "%" (number-to-string w) "d \u2502 ")))
    ad-do-it))
(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)
(global-set-key (kbd "M-9") 'linum-mode)

;; Highlight current line
(global-hl-line-mode)

;; Ido
(ido-mode t)
(icomplete-mode t)
(ido-everywhere t)
(setq ido-virtual-buffers t)
(setq ido-enable-flex-matching t)

;; Ibuffer
(defalias 'list-buffers 'ibuffer)
(setq bs-configurations
      '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))
(global-set-key (kbd "M-1") 'bs-show)
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))

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
(add-to-list 'ac-dictionary-directories (concat user-emacs-directory "elpa/auto-complete/dict/"))

;; Speedbar
(setq speedbar-directory-unshown-regexp "^\\(\\.\\.*$\\)\\'")
(global-set-key (kbd "M-0") 'sr-speedbar-toggle)

;; Bookmark settings
(setq bookmark-save-flag t)
(when (file-exists-p "./.emacs.bookmarks")
  (bookmark-load bookmark-default-file t))
(global-set-key (kbd "M-2") 'bookmark-bmenu-list)
(global-set-key (kbd "M-3") 'bookmark-jump)
(global-set-key (kbd "M-4") 'bookmark-set)
(global-set-key (kbd "M-5") 'bookmark-delete)
(setq bookmark-default-file "./.emacs.bookmarks")

;; Define font
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
      (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; Key bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-M-h") 'windmove-left)
(global-set-key (kbd "C-M-j") 'windmove-down)
(global-set-key (kbd "C-M-k") 'windmove-up)
(global-set-key (kbd "C-M-l") 'windmove-right)

;; Key-chord
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

;; Indent guide
(global-set-key (kbd "M-§") 'indent-guide-global-mode)

;; Rainbow delimiters
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

(custom-set-faces
 '(custom-face-tag ((t (:foreground "blue"))))
 '(custom-variable-tag ((t (:foreground "blue" :weight bold))))
 '(font-lock-comment-delimiter-face ((t (:foreground "brightred"))))
 '(font-lock-comment-face ((t (:foreground "yellow"))))
 '(font-lock-function-name-face ((t (:foreground "blue"))))
 '(font-lock-keyword-face ((t (:foreground "cyan"))))
 '(font-lock-string-face ((t (:foreground "red"))))
 '(hl-line ((t (:background "white"))))
 '(isearch ((t (:background "magenta" :foreground "lightgray"))))
 '(lazy-highlight ((t (:background "green" :foreground "lightgray"))))
 '(linum ((t (:inherit (shadow default) :background "brightwhite"))))
 '(minibuffer-prompt ((t (:foreground "red"))))
 '(mode-line ((t (:background "brightwhite" :foreground "black"))))
 '(mode-line-buffer-id ((t (:foreground "yellow" :weight bold))))
 '(mode-line-inactive ((t (:background "brightwhite" :foreground "yellow"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#e5493d"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#d57807"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#c39900"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#96a700"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#2c9edb"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#2c76db"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#b57cf5"))))
 '(rainbow-delimiters-mismatched-face ((t (:background "#e5493d"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "#e5493d"))))
 '(show-paren-match ((t (:background "black" :foreground "brightwhite"))))
 '(speedbar-directory-face ((t (:foreground "yellow"))))
 '(whitespace-empty ((t (:background "color-250"))))
 '(whitespace-indentation ((t (:background "color-250"))))
 '(whitespace-line ((t nil)))
 '(whitespace-space ((t (:foreground "color-245"))))
 '(whitespace-space-after-tab ((t (:background "color-250"))))
 '(whitespace-tab ((t (:background "color-250")))))

(custom-set-variables
 '(speedbar-show-unknown-files t))
