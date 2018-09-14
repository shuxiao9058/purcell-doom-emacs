;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:

(defconst sea-local-dir
  (expand-file-name ".local/" user-emacs-directory)
  "local directory.")
(defconst sea-cache-dir
  (expand-file-name "cache/" sea-local-dir)
  "Cache directory.")
(defconst sea-etc-dir
  (expand-file-name "etc/" sea-local-dir)
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
(require 'init-package)
(require 'init-basic)
(require 'init-funcs)
(require 'init-evil)
(require 'init-ui)
(require 'init-edit)
(require 'init-highlight)
(require 'init-keybinds)

(require 'init-ivy)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-projectile)
(require 'init-flycheck)

(require 'init-utils)
(require 'init-vcs)
(require 'init-dired)
(require 'init-restore)
(require 'init-org)
(require 'init-eshell)
(require 'init-prog)
(require 'init-emacs-lisp)
(require 'init-lsp)
(require 'init-py)
(require 'init-js)
(require 'init-web)

;; Start server
(require 'server)
(unless (server-running-p)
  (server-start))
  
(dolist (dir (list sea-local-dir sea-cache-dir sea-etc-dir))
        (unless (file-directory-p dir)
          (make-directory dir t)))

(setq custom-file (concat sea-cache-dir "custom.el"))
(load custom-file t t)
