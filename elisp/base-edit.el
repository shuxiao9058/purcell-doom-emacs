

;; quick jump
(use-package avy
  :bind
  ("C-;" . avy-goto-word-or-subword-1))



(require-package 'hungry-delete)
(require 'hungry-delete)
(global-hungry-delete-mode)
(diminish 'hungry-delete-mode)



(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  :diminish
  aggressive-indent-mode
  ;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  )



(require-package 'move-dup)
(require 'move-dup)
(global-set-key [M-up] 'md/move-lines-up)
(global-set-key [M-down] 'md/move-lines-down)
(global-set-key (kbd "C-M-<up>") 'md/duplicate-up)
(global-set-key (kbd "C-M-<down>") 'md/duplicate-down)



(use-package yasnippet
  :diminish (yas-minor-mode . "")
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"                 ;; personal snippets
	  ;; "/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
	  ;; "/path/to/yasnippet/snippets"         ;; the default collection
	  "~/yasnippet-snippets"	;git clone https://github.com/AndreaCrotti/yasnippet-snippets.git
	  ))

  (yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.
  )



(require-package 'multiple-cursors)
(require-package 'visual-regexp)
(require-package 'visual-regexp-steroids)
(require 'visual-regexp-steroids)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
;; to use visual-regexp-steroids's isearch instead of the built-in regexp isearch, also include the following lines:
(define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
(define-key esc-map (kbd "C-s") 'vr/isearch-forward) ;; C-M-s


(use-package expand-region
  :bind
  ("C-=" . er/expand-region))



(use-package flycheck
  :ensure t
  ;; :init (global-flycheck-mode)
  )


(use-package ag)
(use-package wgrep)

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
  :diminish origami-mode
  :bind
  ("C-{" . origami-toggle-node)
  ("C-S-O". origami-toggle-all-nodes))

(use-package smartparens
  :defer t
  :ensure t
  :diminish smartparens-mode
  :init
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (sp-use-smartparens-bindings)
  (sp--update-override-key-bindings)
  :commands (smartparens-mode show-smartparens-mode))

(use-package fix-word
  :config
  (global-set-key (kbd "M-u") #'fix-word-upcase)
  (global-set-key (kbd "M-l") #'fix-word-downcase)
  (global-set-key (kbd "M-c") #'fix-word-capitalize))

(use-package anzu
  :config
  (global-anzu-mode +1)
  :diminish
  anzu-mode)

(provide 'base-edit)
