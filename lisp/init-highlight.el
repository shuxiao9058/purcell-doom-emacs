;; init-highlight.el --- Initialize highlight configurations.	-*- lexical-binding: t -*-
;;; Code:

;; Highlight the current line
; (use-package hl-line
  ; :ensure nil
  ; :init (add-hook 'after-init-hook #'global-hl-line-mode))

;; Highlight symbols
; (use-package symbol-overlay
  ; :diminish symbol-overlay-mode
  ; :bind (("M-i" . symbol-overlay-put)
         ; ([C-f3] . symbol-overlay-put)
         ; ([f3] . symbol-overlay-jump-next)
         ; ([S-f3] . symbol-overlay-jump-prev)
         ; ([M-f3] . symbol-overlay-remove-all))
  ; :init (add-hook 'prog-mode-hook #'symbol-overlay-mode))

;; Highlight matching paren
(use-package paren
  :ensure nil
  :init (add-hook 'after-init-hook #'show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

;; Highlight surrounding parentheses
(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :init (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  :config (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold))

;; Highlight indentions
(use-package indent-guide
  :diminish indent-guide-mode
  :init (add-hook 'after-init-hook #'indent-guide-global-mode)
  :config (setq indent-guide-delay 0.5))

;; Colorize color names in buffers
(use-package rainbow-mode
  :diminish rainbow-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-mode)
  (with-eval-after-load 'web-mode
    (add-hook 'web-mode-hook #'rainbow-mode))
  (with-eval-after-load 'css-mode
    (add-hook 'css-mode-hook #'rainbow-mode)))
;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Highlight TODO/FIXME/BUG...
  (use-package fic-mode
    :config
    (setq fic-activated-faces '(font-lock-comment-face))
    (set-face-background 'fic-face "yellow")
    (set-face-background 'fic-author-face "yellow"))

;; Highlight uncommitted changes
(use-package diff-hl
  :bind (:map diff-hl-command-map
              ("SPC" . diff-hl-mark-hunk))
  :init
  (add-hook 'after-init-hook #'global-diff-hl-mode)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  :config
  (diff-hl-flydiff-mode 1)

  ;; Fall back to the display margin, if the fringe is unavailable
  (unless (display-graphic-p)
    (setq diff-hl-side 'right)
    (diff-hl-margin-mode 1))

  ;; Integration with magit and psvn
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  (with-eval-after-load 'psvn
    (defadvice svn-status-update-modeline (after svn-update-diff-hl activate)
      (diff-hl-update))))

;; Highlight some operations
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :init (add-hook 'after-init-hook #'volatile-highlights-mode))

;; Visualize TAB, (HARD) SPACE, NEWLINE
(use-package whitespace
  :ensure nil
  :diminish whitespace-mode
  :init
  (dolist (hook '(prog-mode-hook outline-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config
  (setq whitespace-line-column fill-column) ;; limit line length
  ;; automatically clean up bad whitespace
  (setq whitespace-action '(auto-cleanup))
  ;; only show bad whitespace
  (setq whitespace-style '(face
                           trailing space-before-tab
                           indentation empty space-after-tab))

  (with-eval-after-load 'popup
    ;; advice for whitespace-mode conflict with popup
    (defvar my-prev-whitespace-mode nil)
    (make-local-variable 'my-prev-whitespace-mode)

    (defadvice popup-draw (before my-turn-off-whitespace activate compile)
      "Turn off whitespace mode before showing autocomplete box."
      (if whitespace-mode
          (progn
            (setq my-prev-whitespace-mode t)
            (whitespace-mode -1))
        (setq my-prev-whitespace-mode nil)))

    (defadvice popup-delete (after my-restore-whitespace activate compile)
      "Restore previous whitespace mode when deleting autocomplete box."
      (if my-prev-whitespace-mode
          (whitespace-mode 1)))))

(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :config
  (add-hook 'prog-mode-hook 'whitespace-cleanup-mode)
  )

(provide 'init-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-highlight.el ends here
