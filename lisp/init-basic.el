;;; init-basic.elAuthor: Haibo Wang <nasoundead@163.com>
;;; Code:
(eval-when-compile (require 'init-const))  
  
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

(when sys/windowsp
  (mapc #'prepend-to-exec-path
        (reverse (list (if sys/win64p
                           "C:/Program Files (x86)/Git/bin"
                         "C:/Program Files/Git/bin")
                       "~/forwin/dll"
                       "~/forwin/bin"
                       ))))

;; Key Modifiers
(when sys/windowsp
  ;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super) ; Left Windows key

  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super) ; Right Windows key

  (setq w32-pass-apps-to-system nil)
  (setq w32-apps-modifier 'hyper) ; Menu/App key
  )

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

;; Environment
(when (or sys/mac-x-p sys/linux-x-p)
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

;; Start server
(require 'server)
(unless (server-running-p)
  (server-start))



;; Show native line numbers if possible, otherwise use linum
(if (version<= "26.0.50" emacs-version )
    (global-display-line-numbers-mode)
  (add-hook 'after-init-hook #'global-linum-mode))

  
;; https://www.emacswiki.org/emacs/AutoSave
(make-directory doom-cache-dir t)

(defun save-all ()
  "Save all buffers."
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode))

(use-package ido-vertical-mode
  :init
  (ido-vertical-mode 1)
  :config
  (setq ido-vertical-show-count 1))


(setq kill-ring-max 200)

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Kill & Mark things easily
(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

;; Interactively insert items from kill-ring
(use-package browse-kill-ring
  :bind ("C-c k" . browse-kill-ring)
  :init (add-hook 'after-init-hook #'browse-kill-ring-default-keybindings))

(provide 'init-basic)
;;; base ends here
