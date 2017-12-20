
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
  "<SPC>" 'counsel-M-x
  "wd" 'delete-window
  "wm" 'delete-other-windows
  "w\\" 'split-window-horizontally-instead
  "w-" 'split-window-vertically-instead
  "0" '(lambda ()
         (interactive)
         (unless (ignore-errors (select-window-0-or-10))
           (treemacs-select-window)))
  "1" 'select-window-1
  "2" 'select-window-2
  "3" 'select-window-3
  "4" 'select-window-4
  "5" 'select-window-5
  "6" 'select-window-6
  "7" 'select-window-7
  "8" 'select-window-8
  "9" 'select-window-9
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

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  )

(require-package 'key-chord)
(setq key-chord-two-keys-delay 0.1) ; default 0.1
(setq key-chord-one-key-delay 0.2) ; default 0.2
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)

(use-package evil-escape
  :ensure t
  :bind(("C-c C-g" . evil-escape))
  :config
  (diminish 'evil-escape-mode))
(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))
(use-package dash
  :ensure t
  :defer t)
(use-package f
  :ensure t
  :defer t)
(use-package s
  :ensure t
  :defer t)
(use-package evil-surround
  :defer t
  :init
  (global-evil-surround-mode)
  :ensure t)

(provide 'init-evil)
