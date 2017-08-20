;; (setq package-enable-at-startup nil)

;; (setq package-archives '(
;; 			 ("melpa-cn" . "http://elpa.zilongshanren.com/melpa/")
;; 			 ("org-cn"   . "http://elpa.zilongshanren.com/org/")
;; 			 ("gnu-cn"   . "http://elpa.zilongshanren.com/gnu/")))
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))
(package-initialize)

(defun config-proxy()
  "config proxy when you are using proxy to access internet"
  (setq url-proxy-services '(("http" . "127.0.0.1:3128")
			     ("https" . "127.0.0.1:3128")))
  (setq url-http-proxy-basic-auth-storage
	(list (list "127.0.0.1:3128"
		    (cons "Input your LDAP UID !"
			  (base64-encode-string "w00220012:huaweier@0"))))))


(when (not package-archive-contents)
  (package-refresh-contents))

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

(require-package 'use-package)

;; ----------------------------------
(defun prepend-to-exec-path (path)
  "prepend the path to the emacs intenral `exec-path' and \"PATH\" env variable.
Return the updated `exec-path'"
  (setenv "PATH" (concat (expand-file-name path)
                         path-separator
                         (getenv "PATH")))
  (setq exec-path
        (cons (expand-file-name path)
              exec-path)))


;; Prepend a path to the begin of the `load-path'
(defun prepend-to-load-path (path)
  "prepend the PATH to the head of the `load-path', return updated load-path."
  (add-to-list 'load-path path))

;; ----------------------------------


(defconst private-dir  (expand-file-name "private" user-emacs-directory)
  "pvrivate dir")
(defconst temp-dir (format "%s/cache" private-dir)
  "Hostname-based elisp temp directories")
(defconst os:windowsp (eq system-type 'windows-nt)
  "if current operation system is windows system")
(defconst os:linuxp (eq system-type 'gnu/linux)
  "if current operation system is linux")
(defconst os:win32p (and os:windowsp
			 (not (getenv "PROGRAMW6432")))
  "if current operation system is windows 32bit version")
(defconst os:win64p (and os:windowsp
			 (getenv "PROGRAMW6432"))
  "if current operation system is windows 64bit verison.")

;; coding
(cond
 ((eq system-type 'windows-nt)
  (set-language-environment "chinese-gbk")
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'gbk)
  (modify-coding-system-alist 'process "*" 'gbk)
  (defun windows-shell-mode-coding ()
    (set-buffer-file-coding-system 'gbk)
    (set-buffer-process-coding-system 'gbk 'gbk))
  (add-hook 'shell-mode-hook #'windows-shell-mode-coding)
  (add-hook 'inferior-python-mode-hook #'windows-shell-mode-coding)
  (defun python-encode-in-org-babel-execute (func body params)
    (let ((coding-system-for-write 'utf-8))
      (funcall func body params)))
  (advice-add #'org-babel-execute:python :around
 	      #'python-encode-in-org-babel-execute))
 (t
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8)))

;; there are difference between exec-path and PATH.
;; The value of environment variable “PATH” is used by emacs when you are running a shell in emacs, similar to when you are using a shell in a terminal.
;; The exec-path is used by emacs itself to find programs it needs for its features, such as spell checking, file compression, compiling, grep, diff, etc. Original from http://ergoemacs.org/emacs/emacs_env_var_paths.html
(when os:windowsp
  (mapc #'prepend-to-exec-path
        (reverse
         (list
          (if os:win64p
              "C:/Program Files (x86)/Git/bin"
            "C:/Program Files/Git/bin")
	  "~/forwin/dll"
	  "~/forwin/bin"
	  ))))


;; Emacs customizations
(setq
 confirm-nonexistent-file-or-buffer  t
 save-interprogram-paste-before-kill t
 mouse-yank-at-point                 t
 require-final-newline               t
 visible-bell                        nil
 ring-bell-function                  'ignore
 custom-file                         "~/.emacs.d/.custom.el"
 minibuffer-prompt-properties
 '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
 cursor-in-non-selected-windows     nil
 highlight-nonselected-windows      nil
 exec-path                          (append exec-path '("/usr/local/bin/"))
 indent-tabs-mode                   nil
 inhibit-startup-message            t
 fringes-outside-margins            t
 x-select-enable-clipboard          t
 use-package-always-ensure          t
 history-length                     300
 backup-inhibited                   nil
 make-backup-files                  nil
 auto-save-default                  nil
 make-backup-files                  nil
 create-lockfiles                   nil
 inhibit-compacting-font-caches     t
 )
(auto-image-file-mode)
(delete-selection-mode 1)
(show-paren-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (custom-set-variables
;;  '(initial-frame-alist (quote ((fullscreen . maximized))))) ;; start maximized
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (  fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(global-set-key (kbd "RET") 'newline-and-indent)

(require 'server)
(unless (server-running-p)
  (server-start))

(use-package recentf
  :config
  (setq recentf-save-file (recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
  (recentf-mode 1))
(use-package dired+
  :init
  (setq diredp-hide-details-initially-flag nil))

(use-package hlinum
  :config
  (hlinum-activate))

(use-package linum
  :config
  (setq linum-format " %2d ")
  (global-linum-mode nil))


(use-package page-break-lines
  :config
  (global-page-break-lines-mode)
  :diminish
  page-break-lines-mode)


;; set default font in initial window and for any new window
(cond
 ;; case: windows
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  ;; Setting English Font
  (set-face-attribute 'default nil :font "Consolas 11")
  ;; Chinese Font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
		      charset (font-spec :family "Microsoft Yahei"
					 :size 16)))
  (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
  (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease))
 ;; case: Max OS X
 ((string-equal system-type "darwin") ; Mac OS X
  (when (member "DejaVu Sans Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-10"))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))))
 ;; case: linux
 ((string-equal system-type "gnu/linux") ; linux
  (when (member "DejaVu Sans Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-10"))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10")))
  (global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
  (global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease))
 )

;; specify font for all unicode characters
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))



(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 9)
			  (bookmarks . 9)
			  (projects . 9))))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))

(use-package exec-path-from-shell
  :config
  ;; Add GOPATH to shell
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-copy-env "PYTHONPATH")
    (exec-path-from-shell-initialize)))

(require-package 'imenu-anywhere)
(require-package 'imenu+)
(require 'imenu+)
(defun try-to-add-imenu ()
  (condition-case nil (imenu-add-to-menubar "Imenu") (error nil)))
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)

(require-package 'sr-speedbar)
(require 'sr-speedbar)

(require-package 'goto-last-change)
(require 'goto-last-change)
(global-set-key (kbd "C-x C-/") 'goto-last-change)

(require-package 'ido-vertical-mode)
(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-vertical-show-count t)

;; https://github.com/joodland/bm
(use-package bm
  :ensure t
  :demand t
  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)
  :config
  ;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers t)
  ;; where to store persistant files
  (setq bm-repository-file "~/.emacs.d/bm-repository")
  ;; save bookmarks
  (setq-default bm-buffer-persistence t)
  ;; Loading the repository from file when on start up.
  (add-hook' after-init-hook 'bm-repository-load)
  ;; Restoring bookmarks when on file find.
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
				 (bm-buffer-save-all)
				 (bm-repository-save)))
  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)
  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)

  :bind (("<f2>" . bm-next)
	 ("S-<f2>" . bm-previous)
	 ("C-<f2>" . bm-toggle))
  )


(use-package undo-tree
  :config
  ;; Remember undo history
  (setq
   undo-tree-auto-save-history nil
   undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/"))))
  (global-undo-tree-mode 1)
  (diminish 'undo-tree-mode))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package projectile
  :config
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" temp-dir))

  (setq projectile-completion-system 'ivy)

  (projectile-global-mode)

  (setq-default
   projectile-mode-line
   '(:eval
     (if (file-remote-p default-directory)
	 " Pr"
       (format " [%s]" (projectile-project-name)))))

  (setq projectile-switch-project-action 'neotree-projectile-action)
  ;; (setq projectile-switch-project-action 'treemacs-projectile)
  )

(use-package projectile-speedbar
  :bind
  ("C-<f8>" . projectile-speedbar-open-current-buffer-in-tree))


(use-package ivy
  :diminish (ivy-mode . "")
  :bind
  ("C-S-s" . swiper)
  ("C-x C-r" . ivy-resume)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers nil)
  )

(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x c k" . counsel-yank-pop)
  ("C-h v" . counsel-describe-variable)
  ("C-h f" . counsel-describe-function)
  )
(use-package smex
  ;; :bind
  ;; ("M-x" . smex)
  :config
  (setq-default smex-save-file (expand-file-name ".smex-items" temp-dir)))

(use-package counsel-projectile
  :bind
  ("C-x v" . counsel-projectile)
  ("C-x c p" . counsel-projectile-ag)
  :config
  (counsel-projectile-on))

(require-package 'neotree)
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq inhibit-compacting-font-caches t)
;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-smart-open t)
(add-hook 'neotree-mode-hook
	  (lambda ()
	    (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
	    (define-key evil-normal-state-local-map (kbd "l") 'neotree-quick-look)
	    (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
	    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
	    (define-key evil-normal-state-local-map (kbd "r") 'neotree-refresh)
	    (define-key evil-normal-state-local-map (kbd "h") 'neotree-select-up-node)
	    (define-key evil-normal-state-local-map (kbd "y") 'neotree-copy-filepath-to-yank-ring)
	    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
	(file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
	(if (neo-global--window-exists-p)
	    (progn
	      (neotree-dir project-dir)
	      (neotree-find file-name)))
      (message "Could not find git project root."))))
;; (global-set-key [f8] 'neotree-project-dir)

(provide 'base)
;;; base ends here
