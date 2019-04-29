;;; init.el --- keoko's emacs configuration

;; disable menu bar, scroll bar and tool bar
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; line number
(setq linum-format "%4d")

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; replace highlighted text with what I type
(delete-selection-mode 1)

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
;; (server-start)

;; highlight the current line
;;(global-hl-line-mode +1)


(show-paren-mode)

;; special chars in mac like @
(setq-default mac-right-option-modifier nil)
;; alt/meta key for international keyboards
;;(setq mac-option-modifier 'none)
;;(setq mac-command-modifier 'meta)

;; Invoke M-x without the Alt key
;; https://sites.google.com/site/steveyegge2/effective-emacs
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)




;; https://sites.google.com/site/steveyegge2/effective-emacs
(defalias 'qrr 'query-replace-regexp)

;; (global-set-key [f5] 'call-last-kbd-macro)
(define-key global-map [?\s-0] 'call-last-kbd-macro)

;; Prefer backward-kill-word over Backspace
;; https://sites.google.com/site/steveyegge2/effective-emacs
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)


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
(define-key global-map [?\s-s] 'swiper)



;; open init.el shortcut
(global-set-key (kbd "C-c e") 
                (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; org-mode
(global-set-key (kbd "C-c o") 
                (lambda () (interactive) (find-file "~/org/organizer.org")))

;; org-mode
(global-set-key (kbd "C-c u") 
                (lambda () (interactive) (find-file "~/org/useful-commands.org")))


;; org-mode
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-default-notes-file "~/org/organizer.org")
(setq org-agenda-files '("~/org/organizer.org"
                         "~/org/work.org"
                         "~/org/clojure.org"
			 "~/org/emacs.org"))
(setq org-refile-targets '(("~/org/organizer.org" :maxlevel . 1)
                         ("~/org/work.org" :maxlevel . 1)
                         ("~/org/clojure.org" :maxlevel . 1)
			 ("~/org/emacs.org" :maxlevel . 1)
			 ("~/org/projects.org" :maxlevel . 2)			 
			 ("~/org/someday.org" :level . 1)))



;;(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/organizer.org" "Tasks")
             "* TODO %?\n")
        ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c)")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode agenda options                                                ;;
;; from http://pragmaticemacs.com/page/11/                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;open agenda in current window
(setq org-agenda-window-setup (quote current-window))
;;warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)
;;show me tasks scheduled or due in next fortnight
(setq org-agenda-span (quote fortnight))
;;don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;;don't give awarning colour to tasks with impending deadlines
;;if they are scheduled to be done
(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
;;don't show tasks that are scheduled or have deadlines in the
;;normal todo list
(setq org-agenda-todo-ignore-deadlines (quote all))
(setq org-agenda-todo-ignore-scheduled (quote all))
;;sort tasks in order of when they are due and then by priority
(setq org-agenda-sorting-strategy
  (quote
   ((agenda deadline-up priority-down)
    (todo priority-down category-keep)
    (tags priority-down category-keep)
    (search category-keep))))


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

;; see https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
;; https://github.com/noctuid/evil-guide
;; see http://makble.com/how-to-toggle-evil-mode-in-emacs in how to toogle evil and non-evil mode
;; load evil
(use-package evil
  :ensure t ;; install the evil package if not installed
  :init	    ;; tweak evil's configuration before loading it
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-move-beyond-eol nil)
  :config ;; tweak evil after loading it
  (evil-mode))

  ;; example how to map a command in normal mode (called 'normal state' in evil)
;;  (define-key evil-normal-state-map (kbd ", w") 'evil-window-vsplit))

(use-package xah-find
  :ensure t)

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark))

(use-package magit
  :ensure t
  :init
  (setq magit-diff-paint-whitespace t)
  (setq magit-diff-highlight-trailing t))

;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (load-theme 'solarized-light t))

(use-package cyberpunk-theme
  :if (window-system)
  :ensure t
  :init
  (progn
    (load-theme 'cyberpunk t)
    (set-face-attribute `mode-line nil
                        :box nil)
    (set-face-attribute `mode-line-inactive nil
                        :box nil)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c m c" . mc/edit-lines)
         ("C-c m n" . mc/mark-next-like-this)
         ("C-c m p" . mc/mark-previous-like-this)
	 ("C-c m a" . mc/mark-all-like-this)))

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
  :bind* (;;("?\s-s" . swiper)
          ;;("M-S" . swiper-all)
          :map swiper-map
          ("C-s" . ivy-previous-history-element)
          ("C-t" . ivy-yank-word)))


(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))


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
  ;; (setq cider-repl-use-pretty-printing t)
  ;; Hide *nrepl-connection* and *nrepl-server* buffers from appearing
  ;; in some buffer switching commands like switch-to-buffer
  (setq nrepl-hide-special-buffers nil)
  ;; integrant-repl
  (setq cider-ns-refresh-before-fn "integrant.repl/suspend"
	cider-ns-refresh-after-fn "integrant.repl/resume"))

(use-package flycheck-joker
  :ensure t)

(global-set-key "\C-c\C-r\C-r" 'cider-refresh)

(setq cider-lein-command "/Users/icabrebarrera/bin/lein")
(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

(use-package eyebrowse
  :ensure t
  :diminish eyebrowse-mode
  :config
  (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
  (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
  (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
  (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t))

;; see https://discuss.ocaml.org/t/using-emacs-for-ocaml-development/726/2
(and (require 'cl)
     (use-package tuareg
       :ensure t
       :config
       (add-hook 'tuareg-mode-hook #'electric-pair-local-mode)
       ;; (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
       (setq auto-mode-alist
             (append '(("\\.ml[ily]?$" . tuareg-mode)
                       ("\\.topml$" . tuareg-mode))
                     auto-mode-alist)))

     ;; Merlin configuration

     (use-package merlin
       :ensure t
       :config
       (add-hook 'tuareg-mode-hook 'merlin-mode)
       (add-hook 'merlin-mode-hook #'company-mode)
       (setq merlin-error-after-save nil))
     
     ;; utop configuration

     (use-package utop
       :ensure t
       :config
       (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
       (add-hook 'tuareg-mode-hook 'utop-minor-mode)))

(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-command "/usr/local/bin/pandoc"))


;; switch to previous buffer
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "C-c b") 'switch-to-previous-buffer)




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

(defun inconsolata ()
  "Set the default font to Inconsolata."
  (interactive)
  (set-default-font "Inconsolata 16"))
(inconsolata)


(defun replace-buffer-string (old-string new-string)
  (goto-char (point-min))
  (while (search-forward old-string nil t)
    (replace-match new-string nil t)))


;; todo - improvement escape single quotes
(defun comma-separated-query-list ()
  "prepares comma separated list of elements"

  (interactive)
  (save-excursion
    (replace-buffer-string "'" "''")
    (let* ((lines (split-string (buffer-string) "\n" t))
	   (items (mapconcat (lambda (l) (concat "'" l "'")) lines ",\n")))
      (kill-region 1 (point-max))
      (insert items))))

(global-set-key "\C-c\C-l" 'comma-separated-query-list)

(defun clj-strings ()
  "prepares list of clj string"

  (interactive)
  (save-excursion
    (replace-buffer-string "\"" "\\\"")
    (let* ((lines (split-string (buffer-string) "\n" t))
	   (items (mapconcat (lambda (l) (concat "\"" l "\"")) lines "\n")))
      (kill-region 1 (point-max))
      (insert items))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-boot-parameters "cider repl -s wait")
 '(custom-safe-themes
   (quote
    ("59e82a683db7129c0142b4b5a35dbbeaf8e01a4b81588f8c163bd255b76f4d21" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(package-selected-packages
   (quote
    (eyebrowse t+ cyberpunk-theme evil flycheck-joker rust-mode easy-kill undo-tree utop merlin tuareg xah-find clj-refactor ace-window avy solarized json-mode magit material-theme solarized-theme markdown-mode ivy counsel which-key multiple-cursors rainbow-mode rainbow-delimiters projectile zenburn-theme use-package cider exec-path-from-shell)))
 '(safe-local-variable-values
   (quote
    ((cider-cljs-lein-repl . "(do (user/go) (user/cljs-repl))")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
(put 'narrow-to-page 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
