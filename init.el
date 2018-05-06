;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 30000000)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init"
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 800000)))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
;; Constants
(require 'init-const)
;; Customization
(require 'init-custom)
;; Packages
(require 'init-package)
(require 'init-basic)
(require 'init-evil)
(require 'init-funcs)
(require 'init-dired)
(require 'init-restore)
(require 'init-ivy)
(require 'init-projectile)
(require 'init-highlight)
(require 'init-ibuffer)
(require 'init-kill-ring)
(require 'init-edit)
(require 'init-modeline)
(require 'init-window)
(require 'init-ui)
;; (require 'init-font)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-org)
(require 'init-eshell)
(require 'init-utils)
;; programing config
(require 'init-vcs)
(require 'init-flycheck)
(require 'init-prog)
(require 'init-emacs-lisp)
(require 'init-lsp)
(require 'init-py)
(require 'init-js)
(require 'init-web)
;; (require 'init-c)
(put 'dired-find-alternate-file 'disabled nil)
