;;; init-ui.el -*- lexical-binding: t; -*-
(defvar sea-init-ui-hook nil
  "ui hook")
(defvar sea-font (font-spec :family "Source Code Pro" :size 15)
  "en font")
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

;(setq sea-font (font-spec :family "Source Code Pro" :size 14))
;(setq sea-cn-font (font-spec :family "Microsoft Yahei" :size 16))

(defun sea/init-ui (&optional frame)
  "Set the theme and load the font, in that order."
  (reapply-themes)
  (condition-case-unless-debug ex
      (when (display-graphic-p)
        (when (fontp sea-font)
          (set-frame-font sea-font nil (if frame (list frame) t))
          (set-face-attribute 'fixed-pitch frame :font sea-font))
        ;; Fallback to `sea-unicode-font' for Unicode characters
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

; (use-package unicode-fonts
  ; :init
  ; (unicode-fonts-setup))
; (require 'font-lock+)

(use-package all-the-icons
  :commands (all-the-icons-octicon all-the-icons-faicon all-the-icons-fileicon
             all-the-icons-wicon all-the-icons-material all-the-icons-alltheicon)
  :init
  (defun sea*disable-all-the-icons-in-tty (orig-fn &rest args)
    (when (display-graphic-p)
      (apply orig-fn args)))
  :config
  (setq inhibit-compacting-font-caches t)
  ;; all-the-icons doesn't work in the terminal, so we "disable" it.
  (dolist (fn '(all-the-icons-octicon all-the-icons-material
                all-the-icons-faicon all-the-icons-fileicon
                all-the-icons-wicon all-the-icons-alltheicon))
    (advice-add fn :around #'sea*disable-all-the-icons-in-tty)))
	
(use-package neotree
  :commands (neotree-show
             neotree-hide
             neotree-toggle
             neotree-dir
             neotree-find
             neo-global--with-buffer
             neo-global--window-exists-p)
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :config
  
  (setq neo-create-file-auto-open nil
        neo-auto-indent-point nil
        neo-autorefresh nil
        neo-mode-line-type 'none
        neo-window-width 28
        neo-show-updir-line nil
        neo-theme 'nerd ; fallback
        neo-banner-message nil
        neo-confirm-create-file #'off-p
        neo-confirm-create-directory #'off-p
        neo-show-hidden-files nil
        neo-keymap-style 'concise
        neo-show-hidden-files t
        neo-hidden-regexp-list
        '(;; vcs folders
          "^\\.\\(?:git\\|hg\\|svn\\)$"
          ;; compiled files
          "\\.\\(?:pyc\\|o\\|elc\\|lock\\|css.map\\|class\\)$"
          ;; generated files, caches or local pkgs
          "^\\(?:node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
          ;; org-mode folders
          "^\\.\\(?:sync\\|export\\|attach\\)$"
          ;; temp files
          "~$"
          "^#.*#$"))

  (after! winner
    (add-to-list 'winner-boring-buffers neo-buffer-name))

  ;; The cursor always sits at bol. `+neotree*fix-cursor' and
  ;; `+neotree*indent-cursor' change that behavior, so that the cursor is always
  ;; on the first non-blank character on the line, in the neo buffer.
  (defun +neotree*fix-cursor (&rest _)
    (with-current-buffer neo-global--buffer
      (+neotree*indent-cursor)))
  (add-hook 'neo-enter-hook #'+neotree*fix-cursor)

  (defun +neotree*indent-cursor (&rest _)
    (beginning-of-line)
    (skip-chars-forward " \t\r"))
  (advice-add #'neotree-next-line :after #'+neotree*indent-cursor)
  (advice-add #'neotree-previous-line :after #'+neotree*indent-cursor))
  
; (use-package powerline
  ; :init
  ; (powerline-default-theme))

  
(provide 'init-ui)
