(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(setq mac-option-modifier 'none)
(setq mac-command-modifier 'meta)

;; start in the default directory
(setq default-directory (concat (getenv "HOME") "/"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
; (el-get 'sync)

(setq my-packages
      (append
       '(ace-jump-mode yaml-mode color-theme color-theme-solarized
		       markdown-mode htmlize
		       clojure-mode cider paredit
		       deft org-mode
		       multiple-cursors highline smex
		       zenburn-theme
		       auto-complete)

       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))

(el-get 'sync my-packages)


(require 'org-mobile)

;; decor
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(if window-system
    (tool-bar-mode -1))
(load-theme 'zenburn t)


;; deft
(setq deft-directory "~/Dropbox/notes")
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)

(let ((destination (if (file-exists-p "~/Dropbox")
                       "~/Dropbox/notes.org"
                     "~/notes.org")))
  (setq org-default-notes-file destination))
(global-set-key (kbd "C-c d") 'deft)


;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/notes")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/notes/inbox.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; backups
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backup"))
      delete-old-versions t
      kept-new-versions 128
      kept-old-versions 128
      version-control t)

;; ace-jump-mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


;; ido-mode
(require 'ido)
(ido-mode t)
(setq ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-enable-flex-matching t
      ido-enable-prefix nil
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t)
(setq ido-everywhere t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; linum
;;(require 'linum)

;;(global-linum-mode t)
;;(setq linum-format "%2d ")





;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c C-s") 'mc/edit-lines)


; smex Alt-x improved
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)




;; clojure
;; where is lein located? If it's not in a "standard path, add a line like this.
(add-to-list 'exec-path "/Users/icabre/bin")



;; Read in PATH from .bash_profile
(if (not (getenv "TERM_PROGRAM"))
    (setenv "PATH"
            (shell-command-to-string "source $HOME/.bash_profile && printf $PATH")))

;; Show parenthesis mode
(show-paren-mode 1)  

;; rainbow delimiters
;;(global-rainbow-delimiters-mode)

;; paredit
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'paredit-mode)
(global-set-key [f7] 'paredit-mode)



;; nrepl
;(add-hook 'cider-repl-mode-hook 'nrepl-turn-on-eldoc-mode)
;(setq nrepl-popup-stacktraces nil)
;(add-to-list 'same-window-buffer-names "*nrepl*")
(add-hook 'cider-mode-hook 'paredit-mode)

;; Auto complete
(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(setq ac-delay 0.0)
;(setq ac-use-quick-help t)
(setq ac-quick-help-delay 0.0)
;(setq ac-use-fuzzy 1)
;(setq ac-auto-start 1)
;(setq ac-auto-show-menu 1)
(ac-config-default)
