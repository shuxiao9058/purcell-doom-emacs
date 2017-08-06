
(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  )
(evil-leader/set-key
  "ff" 'counsel-find-file
  "bb" 'ivy-switch-buffer
  "bp" 'previous-buffer
  "bn" 'next-buffer
  "bd" 'kill-this-buffer
  "<SPC>" 'smex
  "wd" 'delete-window
  "wm" 'delete-other-windows
  "w\\" 'split-window-horizontally-instead
  "w-" 'split-window-vertically-instead
  ;; "0" 'winum-select-window-0-or-10
  "0" '(lambda ()
	 (interactive)
	 (unless (ignore-errors (winum-select-window-0-or-10))
	   (sr-speedbar-select-window)))
  "1" 'winum-select-window-1
  "2" 'winum-select-window-2
  "3" 'winum-select-window-3
  "4" 'winum-select-window-4
  "5" 'winum-select-window-5
  "6" 'winum-select-window-6
  "7" 'winum-select-window-7
  "8" 'winum-select-window-8
  "9" 'winum-select-window-9
  "gs" 'magit-status
  "gx" 'magit-checkout
  "gc" 'magic-commit
  "gp" 'magit-push
  "gu" 'magit-pull
  "ge" 'magit-ediff-resolve
  "gr" 'magit-rebase-interactive

  )

(define-key evil-normal-state-map (kbd "C-k") (lambda ()
                                                (interactive)
                                                (evil-scroll-up 1)))
(define-key evil-normal-state-map (kbd "C-j") (lambda ()
                                                (interactive)
                                                (evil-scroll-down 1)))
(define-key evil-normal-state-map (kbd "[b") 'previous-buffer)
(define-key evil-normal-state-map (kbd "]b") 'next-buffer)
(define-key evil-normal-state-map (kbd "M-.") nil)
;;=========================================================

;;evil-mode, https://bitbucket.org/lyro/evil
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  )


(require-package 'key-chord)
;; Max time delay between two key presses to be considered a key chord
(setq key-chord-two-keys-delay 0.1) ; default 0.1
;; Max time delay between two presses of the same key to be considered a key chord.
;; Should normally be a little longer than `key-chord-two-keys-delay'.
(setq key-chord-one-key-delay 0.2) ; default 0.2
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)
;;=========================================================
;;evil-nerd-commenter
(use-package evil-nerd-commenter
  :ensure t
  :config
  (evil-leader/set-key
    ";" 'evilnc-comment-operator)
  (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-normal-state-map (kbd ",cc") 'evilnc-copy-and-comment-lines)
  (define-key evil-normal-state-map (kbd ",cr") 'comment-or-uncomment-region))

;;=========================================================
;;evil-escape
(use-package evil-escape
  :ensure t
  :bind(("C-c C-g" . evil-escape))
  :config
  (diminish 'evil-escape-mode))
;;=========================================================
;; evil-matchit
(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))
;;=========================================================
(use-package dash
  :ensure t
  :defer t)
(use-package f
  :ensure t
  :defer t)
(use-package s
  :ensure t
  :defer t)
;; (require 'evil-unimpaired)
;;=========================================================


(provide 'base-evil)
