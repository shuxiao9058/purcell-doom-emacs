;; (use-package moe-theme
;;   :defer t
;;   :init
;;   (require 'moe-theme)
;;   (moe-light))
;; (use-package gruvbox-theme
;;   :defer t
;;   :init
;;   (load-theme 'gruvbox t))
;; (load-theme 'manoj-dark t)

;; (require 'hober-theme)
;; (load-theme 'hober t)

(use-package molokai-theme
  :defer t
  :init
  (load-theme 'molokai t))
(require-package 'powerline)
(require 'powerline)
(powerline-default-theme)

(provide 'base-theme)
