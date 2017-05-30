
;; quick jump
(use-package avy
  :bind
  ("C-;" . avy-goto-word-or-subword-1))


;; (use-package company
;;   :diminish company-mode
;;   :config
;;   (add-hook 'after-init-hook 'global-company-mode))

(setq tab-always-indent 'complete)  ;; use 't when company is disabled
(add-to-list 'completion-styles 'initials t)
;; Stop completion-at-point from popping up completion buffers so eagerly
(setq completion-cycle-threshold 5)


(when (maybe-require-package 'company)
  (add-hook 'after-init-hook 'global-company-mode)
  (after-load 'company
    (diminish 'company-mode "CMP")
    (define-key company-mode-map (kbd "M-/") 'company-complete)
    (define-key company-active-map (kbd "M-/") 'company-select-next)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (setq-default company-backends '((company-capf company-dabbrev-code) company-dabbrev)
                  company-dabbrev-other-buffers 'all))
  (global-set-key (kbd "M-C-/") 'company-complete)
  ;;makes completion start automatically rather than waiting for 3 chars / 0.5sec
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0)
  (when (maybe-require-package 'company-quickhelp)
    (add-hook 'after-init-hook 'company-quickhelp-mode))

  (defun sanityinc/local-push-company-backend (backend)
    "Add BACKEND to a buffer-local version of `company-backends'."
    (set (make-local-variable 'company-backends)
         (append (list backend) company-backends))))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 7)
			  (bookmarks . 5)
			  (projects . 5))))

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

;; (require 'golden-ratio)
;; (golden-ratio-mode 1)
;; (diminish 'golden-ratio-mode)
(winner-mode 1)
(require 'window-numbering)
(window-numbering-mode t)
(diminish 'window-numbering-mode)

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package flycheck
  :ensure t
  ;; :init (global-flycheck-mode)
  )

(require-package 'hungry-delete)
(require 'hungry-delete)
(global-hungry-delete-mode)

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  ;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  )

(use-package ag)
(use-package wgrep)
(use-package counsel
  :bind
  ;; ("M-x" . counsel-M-x)
  ("C-x C-m" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x c k" . counsel-yank-pop))

(use-package smex
  :bind
  ("M-x" . smex)
  :config
  (setq-default smex-save-file (expand-file-name ".smex-items" temp-dir)))

(use-package counsel-projectile
  :bind
  ("C-x v" . counsel-projectile)
  ("C-x c p" . counsel-projectile-ag)
  :config
  (counsel-projectile-on))

(use-package ivy
  :diminish (ivy-mode . "")
  :bind
  ("C-s" . swiper)
  ("C-x C-r" . ivy-resume)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers nil)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))


(use-package hlinum
  :config
  (hlinum-activate))

(use-package linum
  :config
  (setq linum-format " %2d ")
  (global-linum-mode nil))

(use-package magit
  :config

  (setq magit-completing-read-function 'ivy-completing-read)

  :bind
  ;; Magic
  ("C-x g s" . magit-status)
  ("C-x g x" . magit-checkout)
  ("C-x g c" . magit-commit)
  ("C-x g p" . magit-push)
  ("C-x g u" . magit-pull)
  ("C-x g e" . magit-ediff-resolve)
  ("C-x g r" . magit-rebase-interactive))

(use-package magit-popup)

;; (use-package multiple-cursors
;;   :bind
;;   ("C-S-c C-S-c" . mc/edit-lines)
;;   ("C->" . mc/mark-next-like-this)
;;   ("C-<" . mc/mark-previous-like-this)
;;   ("C-c C->" . mc/mark-all-like-this))
(require-package 'multiple-cursors)
;; multiple-cursors
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-+") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; From active region to multiple cursors:
(global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)


(use-package org
  :config
  (setq org-directory "~/org-files"
        org-default-notes-file (concat org-directory "/todo.org"))
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda))


;; plantuml
;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))

(setq org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/extra-bin/bin/plantuml.jar"))

;; Enable plantuml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

;; Integration with org-mode
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

(require-package 'neotree)
(require 'neotree)
(global-set-key [f2] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(use-package org-projectile
  :config
  (org-projectile:per-repo)
  (setq org-projectile:per-repo-filename "todo.org"
	org-agenda-files (append org-agenda-files (org-projectile:todo-files))))

;; (require-package 'org-bullets)
;; (require 'org-bullets)
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(use-package page-break-lines)

(use-package projectile
  :config
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" temp-dir))

  (setq projectile-completion-system 'ivy)

  (projectile-global-mode)

  (setq projectile-switch-project-action 'neotree-projectile-action)
  )

(use-package recentf
  :config
  (setq recentf-save-file (recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
  (recentf-mode 1))

(use-package smartparens-config
  :diminish (smartparens-mode . "")
  :ensure smartparens
  :bind
  (:map smartparens-mode-map
	("C-M-a" . sp-beginning-of-sexp)
	("C-M-e" . sp-end-of-sexp)

	("C-M-f" . sp-forward-sexp)
	("C-M-b" . sp-previous-sexp)

	("C-M-n" . sp-next-sexp)
	("C-M-p" . sp-backward-sexp)

	("C-S-f" . sp-forward-symbol)
	("C-S-b" . sp-backward-symbol)

	("C-<right>" . sp-forward-slurp-sexp)
	("M-<right>" . sp-forward-barf-sexp)
	("C-<left>"  . sp-backward-slurp-sexp)
	("M-<left>"  . sp-backward-barf-sexp)

	("C-M-d" . sp-kill-sexp)
	("C-M-w" . sp-copy-sexp)

	("M-<backspace>" . backward-kill-word)
	)
  :init
  (progn
    (show-smartparens-global-mode t)
    (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
    (add-hook 'markdown-mode-hook 'turn-on-smartparens-mode)))


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


(use-package wgrep)

(use-package yasnippet
  :diminish (yas-minor-mode . "")
  :config
  (yas-global-mode 1))

;; (use-package move-dup
;;   :ensure t
;;   :bind
;;   ([M-up] . md/move-lines-up)
;;   ([M-down] . md/move-lines-down)
;;   ("M-S-<down>" . md/duplicate-down)
;;   ("M-S-<up>" . md/duplicate-up))
(require-package 'move-dup)
(require 'move-dup)
(global-set-key [M-up] 'md/move-lines-up)
(global-set-key [M-down] 'md/move-lines-down)
(global-set-key (kbd "C-M-<up>") 'md/duplicate-up)
(global-set-key (kbd "C-M-<down>") 'md/duplicate-down)

(use-package default-text-scale
  :bind
  ("C-M-=" . default-text-scale-increase)
  ("C-M--" . default-text-scale-decrease))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(require-package 'rainbow-mode)
(require 'rainbow-mode)
(add-hook 'after-init-hook 'rainbow-mode)

(use-package highlight-symbol
  :bind
  ([(control f3)] . highlight-symbol)
  ([f3] . highlight-symbol-next)
  ([(shift f3)] . highlight-symbol-prev)
  ([(meta f3)] . highlight-symbol-query-replace))

(use-package indent-guide
  :config
  (indent-guide-global-mode)
  :diminish indent-guide-mode
  )

(use-package origami
  :config
  (global-origami-mode)
  :diminish origami-mode)

(require-package 'all-the-icons)
(require 'all-the-icons)


(require-package 'ido-vertical-mode)
(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-vertical-show-count t)
(require-package 'idomenu)
;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)
;; http://www.reddit.com/r/emacs/comments/21a4p9/use_recentf_and_ido_together/cgbprem
(add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [up] 'previous-history-element)))



;; http://tumashu.github.io/chinese-pyim/
(use-package chinese-pyim
  :ensure t
  :config
  ;; 激活 basedict 拼音词库
  (use-package chinese-pyim-basedict
    :ensure t
    :config (chinese-pyim-basedict-enable))
  (use-package chinese-pyim-greatdict
    :ensure t
    ;; :config (chinese-pyim-greatdict-enable)
    )
  (setq default-input-method "chinese-pyim")
  (setq pyim-default-scheme 'quanpin)
  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
		'(pyim-probe-dynamic-english
		  pyim-probe-isearch-mode
		  pyim-probe-program-mode
		  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
		'(pyim-probe-punctuation-line-beginning
		  pyim-probe-punctuation-after-punctuation))

  ;; 开启拼音搜索功能
  (setq pyim-isearch-enable-pinyin-search t)

  ;; 使用 pupup-el 来绘制选词框
  (setq pyim-page-tooltip 'popup)
  (setq pyim-page-style 'one-line)

  ;; 选词框显示5个候选词
  (setq pyim-page-length 5)

  ;; 让 Emacs 启动时自动加载 pyim 词库
  (add-hook 'emacs-startup-hook
	    #'(lambda () (pyim-restart-1 t)))

  (setq default-input-method "chinese-pyim")
  (global-set-key (kbd "C-\\") 'toggle-input-method)
  :bind
  (("M-j" . pyim-convert-code-at-point) ;与 pyim-probe-dynamic-english 配合
   ("C-;" . pyim-delete-word-from-personal-buffer))

  )

(provide 'base-extensions)
;;; base-extensions.el ends here
