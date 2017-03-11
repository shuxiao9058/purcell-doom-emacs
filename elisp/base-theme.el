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
;; (require 'eclipse-theme)
;; (load-theme 'eclipse t)

;; (load-theme 'leuven t)
;; (use-package molokai-theme
;;   :defer t
;;   :init
;;   (load-theme 'molokai t))

(use-package madhat2r-theme
  :defer t
  :init
  (load-theme 'madhat2r t))
;; (require-package 'powerline)
;; (require 'powerline)
;; (powerline-default-theme)

;; (use-package spacemacs-theme
;;   :defer t
;;   :init
;;   (load-theme 'spacemacs-dark t))
(use-package spaceline
  :defer t
  :init
  (require 'spaceline-config)
  (spaceline-emacs-theme)
  (spaceline-toggle-window-number-off)
  (spaceline-toggle-buffer-size-off))
(provide 'base-theme)
