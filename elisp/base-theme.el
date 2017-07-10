(require-package 'color-theme-sanityinc-solarized)
(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'solarized-theme)
(require-package 'molokai-theme)
(require-package 'zenburn-theme)
(require-package 'github-theme)
(require-package 'doom-themes)
(require-package 'gruvbox-theme)

;; (require 'hober-theme)
;; (load-theme 'hober t)
;; (require 'eclipse-theme)
;; (load-theme 'eclipse t)
;; If you don't customize it, this is the theme you get.
;; (setq-default custom-enabled-themes '(doom-one))

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
  (setq custom-enabled-themes '(solarized-light))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(solarized-dark))
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

(defun doom-one ()
  "Activate github theme."
  (interactive)
  (setq custom-enabled-themes '(doom-one))
  (reapply-themes))

(defun doom-molokai ()
  "Activate github theme."
  (interactive)
  (setq custom-enabled-themes '(doom-molokai))
  (reapply-themes))

(defun gruvbox-theme ()
  "Activate github theme."
  (interactive)
  (setq custom-enabled-themes '(gruvbox))
  (reapply-themes))
;; (use-package gruvbox-theme
;;   :defer t
;;   :init
;;   (load-theme 'gruvbox t))
;; (load-theme 'manoj-dark t)

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

(require-package 'all-the-icons)
(require 'all-the-icons)

(use-package winum
  :init
  (setq winum-auto-setup-mode-line nil)
  (setq winum-keymap
	(let ((map (make-sparse-keymap)))
	  (define-key map (kbd "C-`") 'winum-select-window-by-number)
	  (define-key map (kbd "C-²") 'winum-select-window-by-number)
	  (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
	  (define-key map (kbd "M-1") 'winum-select-window-1)
	  (define-key map (kbd "M-2") 'winum-select-window-2)
	  (define-key map (kbd "M-3") 'winum-select-window-3)
	  (define-key map (kbd "M-4") 'winum-select-window-4)
	  (define-key map (kbd "M-5") 'winum-select-window-5)
	  (define-key map (kbd "M-6") 'winum-select-window-6)
	  (define-key map (kbd "M-7") 'winum-select-window-7)
	  (define-key map (kbd "M-8") 'winum-select-window-8)
	  (define-key map (kbd "M-9") 'winum-select-window-9)
	  map))
  :config
  (winum-mode)
  )
(use-package spaceline
  :config

  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  )
; (use-package spaceline-all-the-icons
  ; :after spaceline
  ; :config
  ; (spaceline-all-the-icons-theme)
  ; (spaceline-all-the-icons--setup-anzu)            ;; Enable anzu searching
  ; (spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
  ; (spaceline-all-the-icons--setup-git-ahead)       ;; Enable # of commits ahead of upstream in git
  ; (spaceline-all-the-icons--setup-paradox)         ;; Enable Paradox mode line
  ; (spaceline-all-the-icons--setup-neotree)         ;; Enable Neotree mode line
  ; )

(provide 'base-theme)
