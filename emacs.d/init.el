;; TODO - autocomplete
;; TODO - helm notes
;; TODO - theme
;; TODO - multicursor
;; TODO - install clojurescript
;; TODO - install magit
;; TODO - pretify XML, json files
;; TODO - rebin top line, bottom line
;; TODO - move shortcuts
;; TODO - check https://github.com/manuel-uberti/.emacs.d
;; TODO - check emacs-live
;; TODO - paredit in nrepl
;; TODO - check batsov video on cider
;; TODO - open init.el with a shortcut

;; disable menu bar, scroll bar and tool bar
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; start server
(server-start)

;; decrease font
(global-set-key (kbd "C--") 'text-scale-decrease)
;; increase font
(global-set-key (kbd "C-+") 'text-scale-increase)

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

(defvar my-packages
  '(;; cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ;; ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    ;; smex

    ;; project navigation
    ;; projectile

    ;; colorful parenthesis matching
    ;; rainbow-delimiters

    ;; edit html tags like sexps
    ;; tagedit

    ;; edit multiple lines at the same time
    ;; multiple-cursors

    ;; git integration
    ;; magit

    ;; Mac shortcut binding
    ;; mac-key-mode
    ;; redo+

    ;; auto-complete
    ;; auto-complete

    ;; clojure refactor
    ;; clj-refactor

    ;; helm
    ;;helm
    ))

my-packages

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

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


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


;;;;;;
;;;; Customization
;;;;;;
;;
;;;; Add a directory to our load path so that when you `load` things
;;;; below, Emacs knows where to look for the corresponding file.
;;(add-to-list 'load-path "~/.emacs.d/customizations")
;;
;;;; Sets up exec-path-from-shell so that Emacs will use the correct
;;;; environment variables
;;(load "shell-integration.el")
;;
;;;; These customizations make it easier for you to navigate files,
;;;; switch buffers, and choose options from the minibuffer.
;;(load "navigation.el")
;;
;;;; These customizations change the way emacs looks and disable/enable
;;;; some user interface elements
;;(load "ui.el")
;;
;;;; These customizations make editing a bit nicer.
;;(load "editing.el")
;;
;;;; Hard-to-categorize customizations
;;(load "misc.el")
;;
;;;; For editing lisps
;;(load "elisp-editing.el")
;;
;;;; Langauage-specific
;;(load "setup-clojure.el")
;;(load "setup-js.el")
;;
;;
;;;; enable mac mode
;;(setq mac-command-modifier 'alt 
;;      mac-option-modifier 'meta
;;      ns-right-alternate-modifier nil)
;;(mac-key-mode 1)
;;
;;;; enable auto-complete
;;(ac-config-default)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (cider exec-path-from-shell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;;;
;; Cider
;;;;

;; lein command
(setq cider-lein-command "/Users/icabrebarrera/bin/lein")
