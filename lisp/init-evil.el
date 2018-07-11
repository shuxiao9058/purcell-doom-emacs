
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
  "ws" 'split-window-horizontally-instead
  "wv" 'split-window-vertically-instead
  "gs" 'magit-status
  )
(evil-define-key 'visual 'global
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)
  
(define-key evil-normal-state-map (kbd "[b") 'previous-buffer)
(define-key evil-normal-state-map (kbd "]b") 'next-buffer)
(define-key evil-normal-state-map (kbd "M-.") nil)

(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package key-chord
:init
:config
(setq key-chord-two-keys-delay 0.1) ; default 0.1
(setq key-chord-one-key-delay 0.2) ; default 0.2)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1))


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
(use-package evil-args
  :init
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  ;; bind evil-forward/backward-args
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg)

  ;; bind evil-jump-out-args
  (define-key evil-normal-state-map "K" 'evil-jump-out-args))
(use-package evil-mc
  :init
  (evil-mc-mode 1))
(provide 'init-evil)
