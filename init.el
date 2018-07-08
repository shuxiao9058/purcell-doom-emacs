;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
; (require 'core (concat user-emacs-directory "core/core"))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(require 'init-const)
(require 'init-package)
(require 'init-basic)
(require 'init-evil)
(require 'init-funcs)
(require 'init-utils)
(require 'init-vcs)
(require 'init-dired)
(require 'init-restore)
(require 'init-projectile)
(require 'init-highlight)

(require 'init-edit)
(require 'init-ui)
;(require 'init-modeline)

(require 'init-company)
(require 'init-yasnippet)
(require 'init-ivy)

(require 'init-org)
(require 'init-eshell)

(require 'init-flycheck)
(require 'init-prog)
(require 'init-emacs-lisp)
(require 'init-lsp)
(require 'init-py)
(require 'init-js)
(require 'init-web)

(setq custom-file (concat doom-cache-dir "custom.el"))
(load custom-file t t)
