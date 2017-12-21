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

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; start server
(server-start)

;; decrease font
(global-set-key (kbd "C--") 'text-scale-decrease)
;; increase font
(global-set-key (kbd "C-+") 'text-scale-increase)
;; comment & uncomment code
(define-key global-map [?\s--] 'comment-line)
(define-key global-map [?\s-d] 'projectile-find-dir)
;(define-key global-map [?\s-e] 'er/expand-region)
(define-key global-map [?\s-f] 'projectile-find-file)
(define-key global-map [?\s-g] 'projectile-grep)
(define-key global-map [?\s-l] 'goto-line)
(define-key global-map [?\s-m] 'magit-status)
(define-key global-map [?\s-w] 'delete-frame)
(define-key global-map [?\s-x] 'exchange-point-and-mark)
(define-key global-map [?\s-p] 'projectile-switch-project)

;; open init.el shortcut
(global-set-key (kbd "C-c e") 
                (lambda () (interactive) (find-file "~/.emacs.d/init.el")))


;; org-mode
(global-set-key (kbd "C-c o") 
                (lambda () (interactive) (find-file "~/org/organizer.org")))
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/org/organizer.org")
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/organizer.org" "Tasks")
             "* TODO %?\n")
        ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
             "* %?\nEntered on %U\n  %i\n  %a")))



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

;(defvar elpa-melpa-stable '("melpa-stable" . "http://stable.melpa.org/packages/"))
;(add-to-list 'package-archives elpa-melpa-stable t)

;; sets the load path
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)


(use-package magit
  :ensure t
  :init
  (setq magit-diff-paint-whitespace t)
  (setq magit-diff-highlight-trailing t))

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t))

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

(use-package ace-window
  :ensure t
  :bind* ("C-x o" . ace-window)
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-word-or-subword-1)
	 ("s-." . avy-goto-word-or-subword-1)))

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

(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode))
  :config (setq-default js-indent-level 2))

(use-package projectile
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

(use-package clj-refactor
  :ensure t
  :init
  (defun enable-clj-refactor-mode ()
    (clj-refactor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c C-R"))
  (add-hook 'clojure-mode-hook 'enable-clj-refactor-mode)
  ;; Don't use prefix notation when cleaning the ns form.
  (setq cljr-favor-prefix-notation nil)
  (setq cljr--debug-mode t))

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
;;  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

  ;; Pretty print in the REPL.
  (setq cider-repl-use-pretty-printing t)
  ;; Hide *nrepl-connection* and *nrepl-server* buffers from appearing
  ;; in some buffer switching commands like switch-to-buffer
  (setq nrepl-hide-special-buffers nil)
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
(use-package exec-path-from-shell
  :config
  (push "HISTFILE" exec-path-from-shell-variables)
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))


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

(add-to-list 'exec-path "/usr/local/bin")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(org-agenda-files (quote ("~/org/organizer.org")))
 '(package-selected-packages
   (quote
    (clj-refactor ace-window avy solarized json-mode magit material-theme solarized-theme markdown-mode ivy counsel which-key multiple-cursors rainbow-mode rainbow-delimiters projectile zenburn-theme use-package cider exec-path-from-shell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
