(setq package-enable-at-startup nil)
(package-initialize)

(setq package-archives '(
      ("melpa-cn" . "http://elpa.zilongshanren.com/melpa/")
      ("org-cn"   . "http://elpa.zilongshanren.com/org/")
      ("gnu-cn"   . "http://elpa.zilongshanren.com/gnu/")))


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

;; 设置编码
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

;; 设置path
;; there are difference between exec-path and PATH.
;; The value of environment variable “PATH” is used by emacs when you are running a shell in emacs, similar to when you are using a shell in a terminal.
;; The exec-path is used by emacs itself to find programs it needs for its features, such as spell checking, file compression, compiling, grep, diff, etc. Original from http://ergoemacs.org/emacs/emacs_env_var_paths.html
;; (cond
;;  ((eq system-type 'windows-nt)
;;   (setenv "PATH" (concat (getenv "PATH") ":/sw/bin"))
;;   (setq exec-path (append exec-path '("/sw/bin")))))
;; add extra binary path
;; it seems the "find" in "unix-utils-bin" works better and the
;; on in the "etc", so we put "ect" after "unix-utils-bin"
(when os:windowsp
  (mapc #'prepend-to-exec-path
        (reverse
         (list
          (if os:win64p
              "C:/Program Files (x86)/Git/bin"
            "C:/Program Files/Git/bin")

	  "~/.emacs.d/extra-bin/dlls"
	  "~/.emacs.d/extra-bin/gnuwin32"
	  "~/.emacs.d/extra-bin/unix-utils-bin"
	  "~/.emacs.d/extra-bin/bin"))))


;; Core settings
;; UTF-8 please
;; (set-charset-priority 'unicode)
;; (setq locale-coding-system   'utf-8)   ; pretty
;; (set-terminal-coding-system  'utf-8)   ; pretty
;; (set-keyboard-coding-system  'utf-8)   ; pretty
;; (set-selection-coding-system 'utf-8)   ; please
;; (prefer-coding-system        'utf-8)   ; with sugar on top
;; (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Emacs customizations
(setq ;; confirm-kill-emacs                  'y-or-n-p
 confirm-nonexistent-file-or-buffer  t
 save-interprogram-paste-before-kill t
 mouse-yank-at-point                 t
 require-final-newline               t
 visible-bell                        nil
 ring-bell-function                  'ignore
 custom-file                         "~/.emacs.d/.custom.el"
 ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
 minibuffer-prompt-properties
 '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)

 ;; Disable non selected window highlight
 cursor-in-non-selected-windows     nil
 highlight-nonselected-windows      nil
 ;; PATH
 exec-path                          (append exec-path '("/usr/local/bin/"))
 indent-tabs-mode                   nil
 inhibit-startup-message            t
 fringes-outside-margins            t
 x-select-enable-clipboard          t
 use-package-always-ensure          t)

(auto-image-file-mode)
;; Bookmarks
(setq
 ;; persistent bookmarks
 bookmark-save-flag                  t
 bookmark-default-file              (concat temp-dir "/bookmarks"))

;; Backups enabled, use nil to disable
(setq
 history-length                     300
 backup-inhibited                   nil
 make-backup-files                  nil
 auto-save-default                  nil
 auto-save-list-file-name           (concat temp-dir "/autosave")
 make-backup-files                  nil
 create-lockfiles                   nil
 backup-directory-alist            `((".*" . ,(concat temp-dir "/backup/")))
 auto-save-file-name-transforms    `((".*" ,(concat temp-dir "/auto-save-list/") t)))

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)

;; Disable toolbar & menubar
;;(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (  fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(show-paren-mode 1)

;; Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; 启动时全屏
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized))))) ;; start maximized

(require 'server)
(unless (server-running-p)
  (server-start))

;; ============================================================
;; Setting English Font
;; (set-face-attribute
;;  'default nil :font "Inziu Iosevka CL 12")
;; (set-face-attribute
;;  'default nil :font "Monaco 10")
;; ;; Setting Chinese Font
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;; 		    charset
;; 		    (font-spec :family "Microsoft Yahei" :size 16)))

;; ;;WIn7下使用Emacs-25会遇到卡顿，解决方法为使用 Microsoft YaHei Mono 字体。
;; (custom-set-faces
;;  '(default ((t (:family "Microsoft YaHei Mono" :foundry "outline" :slant normal :weight normal :size 17)))))
;; ;;如使用Microsoft YaHei Mono，则可使用如下设置
;; ;; Chinese Font
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font) charset (font-spec :family "Microsoft YaHei" :size 14)))
(require-package 'chinese-fonts-setup)
(require 'chinese-fonts-setup)
(chinese-fonts-setup-enable)
(setq cfs-profiles
      '("profile1"))

(provide 'base)
;;; base ends here
