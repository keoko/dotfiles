;;; init.el --- keoko's emacs configuration

;; disable menu bar, scroll bar and tool bar
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; start server
(server-start)

;; decrease font
(global-set-key (kbd "C--") 'text-scale-decrease)
;; increase font
(global-set-key (kbd "C-+") 'text-scale-increase)

;; highlight the current line
(global-hl-line-mode +1)

;; special chars in mac like @
(setq-default mac-right-option-modifier nil)
;; alt/meta key for international keyboards
;;(setq mac-option-modifier 'none)
;;(setq mac-command-modifier 'meta)


;;;;
;; Packages
;;;;
;; define package repositories
(require 'package)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; sets the load path
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package multiple-cursors
  :ensure t)

(use-package auto-complete
  :ensure t
  :init
  (progn
    (auto-complete-mode t))
  :config
  (progn 
    (use-package auto-complete-config)
    (ac-set-trigger-key "TAB")
    (ac-config-default)))

(use-package which-key
  :ensure t
  :init (which-key-mode 1)
  :diminish (which-key-mode . " Ꙍ")
  :config
  (which-key-setup-side-window-right-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.7)
  )

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "") ; does not display ivy in the modeline
  :init (ivy-mode 1)        ; enable ivy globally at startup
  :bind (:map ivy-mode-map  ; bind in the ivy buffer
         ("C-'" . ivy-avy)) ; C-' to ivy-avy
  :config
  (setq ivy-use-virtual-buffers t)   ; extend searching to bookmarks and …
  (setq ivy-height 20)               ; set height of the ivy window
  (setq ivy-display-style 'fancy)
  (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
  )

(use-package counsel
  :ensure t
  :bind*                           ; load counsel when pressed
  (("M-x"     . counsel-M-x)       ; M-x use counsel
   ("C-x C-f" . counsel-find-file) ; C-x C-f use counsel-find-file
   ("C-x C-r" . counsel-recentf)   ; search recently edited files
   ("C-c f"   . counsel-git)       ; search for files in git repo
   ("C-c s"   . counsel-git-grep)  ; search for regexp in git repo
   ("C-c /"   . counsel-ag)        ; search for regexp in git repo using ag
   ("C-c l"   . counsel-locate))   ; search for files or else using locate
  )

(use-package swiper :ensure t
  :bind* (("M-s" . swiper)
          ("M-S" . swiper-all)
          :map swiper-map
          ("C-s" . ivy-previous-history-element)
          ("C-t" . ivy-yank-word)))


(use-package projctile
  :ensure t
  :bind ("s-p" . projectile-command-map)
  :config
  (projectile-global-mode +1))

(use-package rainbow-delimiters
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
;  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  )

(setq cider-lein-command "/Users/icabrebarrera/bin/lein")
(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

(use-package markdown-mode
  :ensure t)


;; (defvar my-packages
;;   '(;; cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet

;;     ;; extra syntax highlighting for clojure
;;     clojure-mode-extra-font-locking

;;     ;; allow ido usage in as many contexts as possible. see
;;     ;; customizations/navigation.el line 23 for a description
;;     ;; of ido
;;     ;; ido-ubiquitous

;;     ;; Enhances M-x to allow easier execution of commands. Provides
;;     ;; a filterable list of possible commands in the minibuffer
;;     ;; http://www.emacswiki.org/emacs/Smex
;;     ;; smex

;;     ;; project navigation
;;     ;; projectile

;;     ;; colorful parenthesis matching
;;     ;; rainbow-delimiters

;;     ;; edit html tags like sexps
;;     ;; tagedit

;;     ;; edit multiple lines at the same time
;;     ;; multiple-cursors

;;     ;; git integration
;;     ;; magit

;;     ;; Mac shortcut binding
;;     ;; mac-key-mode
;;     ;; redo+

;;     ;; auto-complete
;;     ;; auto-complete

;;     ;; clojure refactor
;;     ;; clj-refactor

;;     ;; helm
;;     ;;helm
;;     ))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
;;(if (eq system-type 'darwin)
;;    (add-to-list 'my-packages 'exec-path-from-shell))

;; (dolist (p my-packages)
;;   (when (not (package-installed-p p))
;;     (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (markdown-mode ivy counsel which-key multiple-cursors rainbow-mode rainbow-delimiters projectile zenburn-theme use-package cider exec-path-from-shell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
