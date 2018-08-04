;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:
(require 'package)
(setq package-enable-at-startup nil
      package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
			 ("melpa" . "http://elpa.emacs-china.org/melpa/")
			 ("org" . "http://elpa.emacs-china.org/org/")
			 ("sunrise-commander" . "http://elpa.emacs-china.org/sunrise-commander/")
			 ("user42" . "http://elpa.emacs-china.org/user42/")
			 ))
(package-initialize)
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(use-package diminish)

(defconst sea-cache-dir
  (expand-file-name ".cache/" user-emacs-directory)
  "Cache directory.")
(defconst sea-etc-dir
  (expand-file-name ".etc/" user-emacs-directory)
  "etc directory.")
(defconst IS-MAC
  (eq system-type 'darwin)
  "Are we running on a Mac system?")
(defconst IS-LINUX
  (eq system-type 'gnu/linux)
  "Are we running on a Linux system?")
(defconst IS-WIN
  (eq system-type 'windows-nt)
  "Are we running on a Linux system?")
(defvar sea-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all sea functions will be verbose. Set DEBUG=1 in the command
line or use --debug-init to enable this.")
(defvar sea-project-hook nil
  "Hook run when a project is enabled. The name of the project's mode and its
state are passed in.")


(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(require 'init-basic)
(require 'init-funcs)
(require 'init-evil)
(require 'init-ui)
(require 'init-edit)
(require 'init-keybinds)
(require 'init-company)

					; (require 'init-utils)
					; (require 'init-vcs)
					; (require 'init-dired)
					; (require 'init-restore)
					; (require 'init-projectile)
					; (require 'init-highlight)

					; 

					; 
					; (require 'init-yasnippet)
					; (require 'init-ivy)

					; (require 'init-org)
					; (require 'init-eshell)

					; (require 'init-flycheck)
					; (require 'init-prog)
					; (require 'init-emacs-lisp)
					; (require 'init-lsp)
					; (require 'init-py)
					; (require 'init-js)
					; (require 'init-web)

;; Start server
(require 'server)
(unless (server-running-p)
  (server-start))
  
(dolist (dir (list sea-cache-dir sea-etc-dir))
        (unless (file-directory-p dir)
          (make-directory dir t)))

(setq custom-file (concat sea-cache-dir "custom.el"))
(load custom-file t t)
