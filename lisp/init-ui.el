;;; init-ui.el -*- lexical-binding: t; -*-
(defvar sea-init-ui-hook nil
  "ui hook")
(defvar sea-font (font-spec :family "Source Code Pro" :size 14)
  "en font")
(defvar sea-cn-font (font-spec :family "Microsoft Yahei" :size 16)
  "cn font")
(defvar sea-unicode-font nil
  "unicode font")
(defvar sea-variable-pitch-font nil
  "variable-pitch font")

;; Title
(setq frame-title-format
      '("GNU Emacs " emacs-version "@" user-login-name " : "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq icon-title-format frame-title-format)


(setq custom-safe-themes t)
(use-package doom-themes)
(use-package color-theme-sanityinc-tomorrow)
(setq-default custom-enabled-themes '(doom-one))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)
(defun zenburn ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(zenburn))
  (reapply-themes))

(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))


;; undo/redo changes to Emacs' window layout
(defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
(autoload 'winner-mode "winner" nil t)
(add-hook 'sea-init-ui-hook #'winner-mode)

;; highlight matching delimiters
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)
(add-hook 'sea-init-ui-hook #'show-paren-mode)

;; Restore old window configurations
(use-package winner
  :ensure nil
  :init
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                "*esh command on file*")))

(setq sea-font (font-spec :family "Source Code Pro" :size 14))
(setq sea-cn-font (font-spec :family "Microsoft Yahei" :size 16))
(defun sea/init-ui (&optional frame)
  "Set the theme and load the font, in that order."
  (reapply-themes)
  (condition-case-unless-debug ex
      (when (display-graphic-p)
        (when (fontp sea-font)
          (set-frame-font sea-font nil (if frame (list frame) t))
          (set-face-attribute 'fixed-pitch frame :font sea-font))
        ;; Fallback to `sea-unicode-font' for Unicode characters
		(when (fontp sea-cn-font)
          (set-fontset-font t 'chinese-gbk sea-cn-font frame))
        (when (fontp sea-unicode-font)
          (set-fontset-font t 'unicode sea-unicode-font frame))
        ;; ...and for variable-pitch-mode:
        (when (fontp sea-variable-pitch-font)
          (set-face-attribute 'variable-pitch frame :font sea-variable-pitch-font)))
    ('error
     (if (string-prefix-p "Font not available: " (error-message-string ex))
         (lwarn 'sea-ui :warning
                "Could not find the '%s' font on your system, falling back to system font"
                (font-get (caddr ex) :family))
       (lwarn 'sea-ui :error
              "Unexpected error while initializing fonts: %s"
              (error-message-string ex)))))
  (run-hooks 'sea-init-ui-hook))
  
(add-hook 'after-init-hook #'sea/init-ui)

(use-package switch-window
:config
(setq-default switch-window-shortcut-style 'alphabet)
(setq-default switch-window-timeout nil)
(global-set-key (kbd "C-x o") 'switch-window))

(use-package windmove
  :ensure nil
  :init (add-hook 'sea-init-ui-hook #'windmove-default-keybindings))

;; Zoom window like tmux
(use-package zoom-window
  :bind ("C-x C-z" . zoom-window-zoom)
  :init (setq zoom-window-mode-line-color "DarkGreen"))
  
(provide 'init-ui)
