(eval-when-compile
  (require 'init-const)
  (require 'init-custom))
;; Logo
(setq fancy-splash-image my-logo)

;; Title
(setq frame-title-format
      '("GNU Emacs " emacs-version "@" user-login-name " : "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq icon-title-format frame-title-format)

;; Menu/Tool/Scroll bars
(unless sys/mac-x-p
  (when (and (fboundp 'menu-bar-mode) menu-bar-mode)
    (menu-bar-mode -1)))
(when (and (fboundp 'tool-bar-mode) tool-bar-mode)
  (tool-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) scroll-bar-mode)
  (scroll-bar-mode -1))

;; Theme
(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'molokai-theme)
(require-package 'monokai-theme)
(require-package 'dracula-theme)
(require-package 'material-theme)
(require-package 'doom-themes)
(require-package 'ample-theme)
(require-package 'nord-theme)
(require-package 'color-theme-modern)
(require-package 'twilight-bright-theme)
(cond
 ((eq my-theme 'default)
  ;; (require 'eclipse-theme)
  ;; (load-theme 'eclipse t)
  (use-package spacemacs-theme
    :init (load-theme 'spacemacs-dark t))
  )
 ((eq my-theme 'monokai)
  (use-package monokai-theme
    :init (load-theme 'monokai t)))
 ((eq my-theme 'dark)
  (use-package spacemacs-theme
    :init (load-theme 'spacemacs-dark t)))
 ((eq my-theme 'light)
  (use-package spacemacs-theme
    :init (load-theme 'spacemacs-light t)))
 ((eq my-theme 'daylight)
  (use-package leuven-theme
    :init (load-theme 'leuven t))))



;; (require 'eclipse-theme)
;; (load-theme 'eclipse t)
;; If you don't customize it, this is the theme you get.
;; (setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))
;; (setq-default custom-enabled-themes '(ample))
;; (setq-default custom-enabled-themes '(monokai))
;; (setq-default custom-enabled-themes '(leuven))
;; (setq-default custom-enabled-themes '(eclipse))
;; (setq-default custom-enabled-themes '(material))
;; (setq-default custom-enabled-themes '(material-light))
;; (require 'darknaso-theme)
;; (setq-default custom-enabled-themes '(darknaso))
;; (setq-default custom-enabled-themes '(dracula))
;; (setq-default custom-enabled-themes '(zenburn))
;; (setq-default custom-enabled-themes '(twilight-bright))
;; (setq-default custom-enabled-themes '(nord))
;; (setq nord-comment-brightness 15)
;; (setq-default custom-enabled-themes '(doom-molokai))

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

(defun theme-leuven()
  "Activate junio theme."
  (interactive)
  (setq custom-enabled-themes '(leuven))
  (reapply-themes))


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
  (winum-mode))

;; Modeline
(use-package spaceline-config
  :ensure spaceline
  :commands (spaceline-spacemacs-theme
             spaceline-info-mode)
  :init
  (setq powerline-default-separator (if sys/windowsp 'arrow 'utf-8))
  (add-hook 'after-init-hook
            (lambda ()
              (spaceline-spacemacs-theme)))
  :config
  (spaceline-info-mode 1)
  (set 'spaceline-highlight-face-func 'spaceline-highlight-face-evil-state))

;; Fonts
(use-package cnfonts
  :init
  (when my-cnfonts-enabled
    (add-hook 'after-init-hook #'cnfonts-enable))
  :config
  (setq cnfonts-keep-frame-size nil)
  (setq cnfonts-profiles
        '("program" "org-mode" "read-book"))
  (setq cnfonts--profiles-steps '(("program" . 3)
                                  ("org-mode" . 6)
                                  ("read-book" . 8))))

;; Line and Column
(setq-default fill-column 80)
(setq column-number-mode t)
(setq line-number-mode t)

;; Show native line numbers if possible, otherwise use linum
(if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :ensure nil
      ;; :init (add-hook 'prog-mode-hook #'display-line-numbers-mode)
      )
  (use-package linum-off
    :after linum
    :init (add-hook 'after-init-hook #'global-linum-mode)
    :config (setq linum-format "%4d ")))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 5
      scroll-margin 5
      scroll-conservatively 100000)

(use-package smooth-scrolling
  :init (add-hook 'after-init-hook #'smooth-scrolling-mode)
  :config (setq smooth-scroll-margin 0))

;; Display Time
(use-package time
  :ensure nil
  :unless (display-graphic-p)
  :preface
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t)
  :init (add-hook 'after-init-hook #'display-time-mode))

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq visible-bell t)
(size-indication-mode 1)
;; (blink-cursor-mode -1)
(setq track-eol t)                      ; Keep cursor at end of lines. Require line-move-visual is nil.
(setq line-move-visual nil)

;; Don't open a file in a new frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; Toggle fullscreen
(bind-keys ([(control f11)] . toggle-frame-fullscreen)
           ([(control super f)] . toggle-frame-fullscreen) ; Compatible with macOS
           ([(super return)] . toggle-frame-fullscreen)
           ([(meta shift return)] . toggle-frame-fullscreen))

(provide 'init-ui)
