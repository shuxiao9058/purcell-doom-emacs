(require-package 'color-theme-sanityinc-solarized)
(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'molokai-theme)
(require-package 'monokai-theme)
(require-package 'dracula-theme)
(require-package 'material-theme)
(require-package 'ample-theme)
(require-package 'color-theme-modern)

;; (require 'hober-theme)
;; (load-theme 'hober t)
;; (require 'eclipse-theme)
;; (load-theme 'eclipse t)
;; If you don't customize it, this is the theme you get.
;; (setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))
;; (setq-default custom-enabled-themes '(ample))
;; (setq-default custom-enabled-themes '(monokai))
;; (setq-default custom-enabled-themes '(leuven))
;; (setq-default custom-enabled-themes '(eclipse))
;; (setq-default custom-enabled-themes '(material))
;; (require 'darknaso-theme)
;; (setq-default custom-enabled-themes '(darknaso))
(setq-default custom-enabled-themes '(dracula))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)
(setq custom-safe-themes t)

(defun theme-molokai ()
  "Activate molokai theme."
  (interactive)
  (setq custom-enabled-themes '(molokai))
  (reapply-themes))

(defun theme-material ()
  "Activate material theme."
  (interactive)
  (setq custom-enabled-themes '(material))
  (reapply-themes))

(defun theme-ample ()
  "Activate ample theme."
  (interactive)
  (setq custom-enabled-themes '(ample))
  (reapply-themes))
(defun theme-dracula ()
  "Activate molokai theme."
  (interactive)
  (setq custom-enabled-themes '(dracula))
  (reapply-themes))

(defun theme-dracula()
  "Activate junio theme."
  (interactive)
  (setq custom-enabled-themes '(dracula))
  (reapply-themes))

(defun theme-leuven()
  "Activate junio theme."
  (interactive)
  (setq custom-enabled-themes '(leuven))
  (reapply-themes))

;; (require-package 'all-the-icons)
;; (require 'all-the-icons)

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
  (winner-mode 1)
  )
(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (set 'spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (setq powerline-default-separator 'wave)
  (spaceline-compile)
  )

(provide 'base-theme)
