
;; (use-package gruvbox-theme
;;   :defer t
;;   :init
;;   (load-theme 'gruvbox t))
;; (load-theme 'manoj-dark t)

(require 'hober-theme)
(load-theme 'hober t)
;; (require 'eclipse-theme)
;; (load-theme 'eclipse t)

;; (load-theme 'leuven t)
;; (load-theme 'manoj-dark t)

;; (use-package molokai-theme
;;   :defer t
;;   :init
;;   (load-theme 'molokai t))
;; (use-package gruber-darker-theme
;;   :defer t
;;   :init
;;   (load-theme 'gruber-darker t))
;; (use-package zenburn-theme
;;   :defer t
;;   :init
;;   (load-theme 'zenburn t))

;; (use-package telephone-line
;;   :init
;;   (setq telephone-line-lhs
;;         '((evil   . (telephone-line-evil-tag-segment))
;;           (accent . (telephone-line-vc-segment
;;                      telephone-line-erc-modified-channels-segment
;;                      telephone-line-process-segment))
;;           (nil    . (telephone-line-minor-mode-segment
;;                      telephone-line-buffer-segment))))
;;   (setq telephone-line-rhs
;;         '((nil    . (telephone-line-misc-info-segment))
;;           (accent . (telephone-line-major-mode-segment))
;;           (evil   . (telephone-line-airline-position-segment))))
;;   (telephone-line-mode t))

(provide 'base-theme)
