(require-package 'color-theme-sanityinc-solarized)
(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'molokai-theme)
(require-package 'zenburn-theme)
(require-package 'github-theme)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(github))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)
(setq custom-safe-themes t)

;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-solarized-light))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-solarized-dark))
  (reapply-themes))

(defun molokai ()
  "Activate molokai theme."
  (interactive)
  (setq custom-enabled-themes '(molokai))
  (reapply-themes))

(defun zenburn ()
  "Activate zenburn theme."
  (interactive)
  (setq custom-enabled-themes '(zenburn))
  (reapply-themes))

(defun github ()
  "Activate github theme."
  (interactive)
  (setq custom-enabled-themes '(github))
  (reapply-themes))
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
