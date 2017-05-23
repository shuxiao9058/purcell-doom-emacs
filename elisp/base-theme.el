;; (use-package material-theme
;;   :defer t
;;   :init
;;   (load-theme 'material))
;; (require-package 'color-theme-sanityinc-tomorrow)
;; (require 'color-theme-sanityinc-tomorrow)
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
;; (use-package gruber-darker-theme
;;   :defer t
;;   :init
;;   (load-theme 'gruber-darker t))
;; (use-package zenburn-theme
;;   :defer t
;;   :init
;;   (load-theme 'zenburn t))
;; (use-package github-theme
;;   :defer t
;;   :init
;;   (load-theme 'github))

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

(require-package 'doom-themes)
(require 'doom-themes)

;;; Settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t  ; if nil, italics is universally disabled

      ;; doom-one specific settings
      doom-one-brighter-modeline nil
      doom-one-brighter-comments nil)

;; Load the theme (doom-one, doom-dark, etc.)
(load-theme 'doom-one t)

;;; OPTIONAL
;; brighter source buffers (that represent files)
(add-hook 'find-file-hook #'doom-buffer-mode-maybe)
;; ...if you use auto-revert-mode
(add-hook 'after-revert-hook #'doom-buffer-mode-maybe)
;; And you can brighten other buffers (unconditionally) with:
(add-hook 'ediff-prepare-buffer-hook #'doom-buffer-mode)

;; brighter minibuffer when active
(add-hook 'minibuffer-setup-hook #'doom-brighten-minibuffer)

;; Enable custom neotree theme
(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

;; Enable nlinum line highlighting
(doom-themes-nlinum-config)   ; requires nlinum and hl-line-mode

(provide 'base-theme)
