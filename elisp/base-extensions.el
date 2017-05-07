;; quick jump
(use-package avy
  :bind
  ("C-;" . avy-goto-char))


(use-package company
  :diminish company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode))

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

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this))


(use-package org
  :config
  (setq org-directory "~/org-files"
        org-default-notes-file (concat org-directory "/todo.org"))
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda))

(use-package org-projectile
  :config
  (org-projectile:per-repo)
  (setq org-projectile:per-repo-filename "todo.org"
	org-agenda-files (append org-agenda-files (org-projectile:todo-files))))


(use-package page-break-lines)

(use-package projectile
  :config
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" temp-dir))

  (setq projectile-completion-system 'ivy)

  (projectile-global-mode)

;;  (setq projectile-switch-project-action 'neotree-projectile-action)
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
	("M-d"   . sp-backward-kill-sexp)
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

(use-package move-dup
  :bind
  ([M-up] . md/move-lines-up)
  ([M-down] . md/move-lines-down)
  ("M-S-<down>" . md/duplicate-down)
  ("M-S-<up>" . md/duplicate-up))

(use-package default-text-scale
  :bind
  ("C-M-=" . default-text-scale-increase)
  ("C-M--" . default-text-scale-decrease)
  )

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

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

(provide 'base-extensions)
;;; base-extensions.el ends here
