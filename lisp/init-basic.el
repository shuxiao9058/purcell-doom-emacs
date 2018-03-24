;;; init-basic.elAuthor: Haibo Wang <nasoundead@163.com>
;; Version: 0.0.1
;; URL: https://github.com/nasoundead/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Package configurations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(eval-when-compile (require 'init-const))
(eval-when-compile (require 'init-custom))
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

;; The value of environment variable 'PATH' is used by emacs when you are running a shell in emacs, similar to when you are using a shell in a terminal.
;; The exec-path is used by emacs itself to find programs it needs for its features, such as spell checking, file compression, compiling, grep, diff, etc.
;; Original from http://ergoemacs.org/emacs/emacs_env_var_paths.html
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

;; History
;; Emacsag 25 has a proper mode for `save-place'
(add-hook 'after-init-hook #'save-place-mode)

(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 200)
  (add-hook 'find-file-hook (lambda ()
                              (unless recentf-mode
                                (recentf-mode)
                                (recentf-track-opened-file))))
  :config
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude "bookmarks")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'"))

(use-package savehist
  :ensure nil
  :init
  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
        history-length 1000
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 60)
  (add-hook 'after-init-hook #'savehist-mode))

;; https://www.emacswiki.org/emacs/AutoSave
(make-directory cache-dir t)
(setq backup-directory-alist
      `((".*" . ,cache-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,cache-dir t)))
(setq auto-save-list-file-prefix
      cache-dir)
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

(use-package all-the-icons
  :init
  (setq inhibit-compacting-font-caches t))

;; use browser depending on url
(setq
 browse-url-browser-function
 '(
   ("wikipedia\\.org" . browse-url-firefox)
   ("github" . browse-url-chromium)
   ("thefreedictionary\\.com" . eww-browse-url)
   ("." . browse-url-default-browser)
   ))

(provide 'init-basic)
;;; base ends here
