 ;; (use-package solarized-theme
 ;;  :defer t
 ;;  :init
 ;;  (load-theme 'solarized-light))
(require-package 'color-theme-sanityinc-tomorrow)
;; (color-theme-sanityinc-tomorrow-eighties)
 ;; (use-package color-theme-sanityinc-tomorrow
 ;;  :defer t
 ;;  :init
 ;;  (sanityinc-tomorrow-night-eighties))
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

;; (use-package madhat2r-theme
;;   :defer t
;;   :init
;;   (load-theme 'madhat2r t))
;; (require-package 'powerline)
;; (require 'powerline)
;; ;; (powerline-default-theme)
;; (powerline-center-evil-theme)

;; (use-package spacemacs-theme
;;   :defer t
;;   :init
;;   (load-theme 'spacemacs-dark t))
;; (use-package spaceline
;;   :defer t
;;   :init
;;   (require 'spaceline-config)
;;   (spaceline-spacemacs-theme)
;;   (spaceline-toggle-window-number-off)
;;   ; (spaceline-toggle-buffer-size-off)
;;   )
(use-package telephone-line
  :init
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-minor-mode-segment
                     telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment))))

  (telephone-line-mode t))
(provide 'base-theme)
