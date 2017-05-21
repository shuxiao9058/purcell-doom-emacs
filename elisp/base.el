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

(defconst private-dir  (expand-file-name "private" user-emacs-directory))
(defconst temp-dir (format "%s/cache" private-dir)
  "Hostname-based elisp temp directories")

;; 设置编码
(cond
 ((eq system-type 'windows-nt)
  (set-language-environment "chinese-gbk")
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'gbk)
  (modify-coding-system-alist 'process "*" 'gbk)
  (defun liu233w/windows-shell-mode-coding ()
    (set-buffer-file-coding-system 'gbk)
    (set-buffer-process-coding-system 'gbk 'gbk))
  (add-hook 'shell-mode-hook #'liu233w/windows-shell-mode-coding)
  (add-hook 'inferior-python-mode-hook #'liu233w/windows-shell-mode-coding)
  (defun liu233w//python-encode-in-org-babel-execute (func body params)
    (let ((coding-system-for-write 'utf-8))
      (funcall func body params)))
  (advice-add #'org-babel-execute:python :around
 	      #'liu233w//python-encode-in-org-babel-execute))
 (t
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8)))

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

;; ============================================================
;; Setting English Font
;; (set-face-attribute
;;  'default nil :font "Inziu IosevkaCC Slab SC 12")
;; (set-face-attribute
;;  'default nil :font "Monaco 10")
;; ;; Setting Chinese Font
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;; 		    charset
;; 		    (font-spec :family "Microsoft Yahei" :size 16)))

;; (set-face-attribute
;;  'default nil :font "Consolas 10")

;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;; 		    charset
;; 		    (font-spec :family "Microsoft Yahei" :size 14)))
;; (setq face-font-rescale-alist '(("微软雅黑" . 1.2) ("Microsoft Yahei" . 1.2) ("WenQuanYi Zen Hei" . 1.2)))

;;WIn7下使用Emacs-25会遇到卡顿，解决方法为使用 Microsoft YaHei Mono 字体。
(custom-set-faces
 '(default ((t (:family "Microsoft YaHei Mono" :foundry "outline" :slant normal :weight normal :height 98 :width normal)))))
;;如使用Microsoft YaHei Mono，则可使用如下设置
;; Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset (font-spec :family "Microsoft YaHei" :size 14)))

(provide 'base)
;;; base ends here
