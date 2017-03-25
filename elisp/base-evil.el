;;=========================================================
					;evil-leader, https://github.com/cofi/evil-leader.git
;; (require 'evil-leader)
;; (global-evil-leader-mode)
;; (evil-leader/set-leader "<SPC>")
					;(evil-leader/set-key
					;  "f" 'find-file
					;  "b" 'switch-to-buffer
					;  "k" 'kill-buffer)
;; (evil-leader/set-key-for-mode 'emacs-lisp-mode "b" 'byte-compile-file)
(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
)
;; (evil-leader/set-key "SPC" 'counsel-M-x)
;; (evil-leader/set-key "bb" 'ivy-switch-buffer)
;; (evil-leader/set-key "ff" 'counsel-find-file)
(evil-leader/set-key
  "ff" 'counsel-find-file
  "bb" 'ivy-switch-buffer
  "bp" 'previous-buffer
  "bn" 'next-buffer
  "bd" 'kill-this-buffer
  "<SPC>" 'counsel-M-x
  "wd" 'delete-window
  "wm" 'delete-other-windows
  "w/" 'split-window-horizontally-instead
  "w-" 'split-window-vertically-instead
  "1" 'select-window-1
  "2" 'select-window-2
  "3" 'select-window-3
  "4" 'select-window-4
  "5" 'select-window-5
  "6" 'select-window-6
  "7" 'select-window-7
  "8" 'select-window-8
  "9" 'select-window-9
  )
;;=========================================================

;;evil-mode, https://bitbucket.org/lyro/evil
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

;;=========================================================
;;evil-org-mode , git clone git://github.com/edwtjo/evil-org-mode.git
;; Here are the keys introduced by evil-org
;; gh	outline-up-heading
;; gj	org-forward-heading-same-level
;; gk	org-backward-heading-same-level
;; gl	outline-next-visible-heading
;; t	org-todo
;; T	org-insert-todo-heading nil
;; H	org-shiftleft
;; J	org-shiftdown
;; K	org-shiftup
;; L	org-shiftright
;; o	always-insert-item
;; O	org-insert-heading
;; ¡¯$¡¯	org-end-of-line
;; ¡¯^¡¯	org-beginning-of-line
;; <	org-metaleft
;; >	org-metaright
;; <leader>a	org-agenda
;; <leader>t	org-show-todo-tree
;; <leader>c	org-archive-subtree
;; <leader>l	evil-org-open-links
;; <leader>o	evil-org-recompute-clocks
;; TAB	org-cycle
;; M-l	org-metaright
;; M-h	org-metaleft
;; M-k	org-metaup
;; M-j	org-metadown
;; M-L	org-shiftmetaright
;; M-H	org-shiftmetaleft
;; M-K	org-shiftmetaup
;; M-J	org-shiftmetadown
;; M-o	org-insert-heading+org-metaright
;; M-t	org-insert-todo-heading nil+ org-metaright
(use-package evil-org
  :ensure t)
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
(use-package dash
  :ensure t
  :defer t)
(use-package f
  :ensure t
  :defer t)
;; (require 'evil-unimpaired)
;;=========================================================


(provide 'base-evil)
