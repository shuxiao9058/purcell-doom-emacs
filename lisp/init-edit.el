;; init-edit.el --- Initialize edit configurations.	-*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(eval-when-compile (require 'init-const))

;; Miscs
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(delete-selection-mode 1)
(setq-default major-mode 'text-mode)
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
(add-hook 'abbrev-mode-hook (lambda () (diminish 'abbrev-mode)))

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)
  
;; revert buffers for changed files
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;; enabled by default in Emacs 25+. No thanks.
(electric-indent-mode -1)

;; savehist / saveplace
(setq savehist-file (concat doom-cache-dir "savehist")
      savehist-save-minibuffer-history t
      savehist-autosave-interval nil ; save on kill only
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      save-place-file (concat doom-cache-dir "saveplace"))
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'save-place-mode)

 ;; Keep track of recently opened files
(use-package recentf
  :init
  (add-hook 'find-file-hook (lambda ()
                              (unless recentf-mode
                                (recentf-mode)
                                (recentf-track-opened-file))))
  :config
  (setq recentf-save-file (concat doom-cache-dir "recentf")
        recentf-max-menu-items 0
        recentf-max-saved-items 300
		recentf-filename-handlers '(file-truename)
        recentf-exclude
        (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
              "^/var/folders/.+$"
              ;; ignore private DOOM temp files (but not all of them)
              (concat "^" (file-truename doom-cache-dir)))))
  
(use-package savehist
  :ensure nil
  :init
  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
        history-length 1000
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 60)
  (add-hook 'after-init-hook #'savehist-mode))
  
;; History
;; Emacsag 25 has a proper mode for `save-place'
(add-hook 'after-init-hook #'save-place-mode)

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :init
  (add-hook 'after-init-hook #'global-aggressive-indent-mode)

  ;; FIXME: Disable in big files due to the performance issues
  ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
  (add-hook 'find-file-hook
            (lambda ()
              (if (> (buffer-size) (* 3000 80))
                  (aggressive-indent-mode -1))))
  :config
  ;; Disable in some modes
  (dolist (mode '(web-mode html-mode css-mode robot-mode python-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (or (derived-mode-p 'c-mode)
             (derived-mode-p 'c++-mode)
             (derived-mode-p 'csharp-mode)
             (derived-mode-p 'java-mode)
             (derived-mode-p 'go-mode)
             (derived-mode-p 'python-mode)
             (derived-mode-p 'swift-mode))
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

;; Show number of matches in mode-line while searching
(use-package visual-regexp-steroids
  :init
  (use-package visual-regexp)
  :bind (([remap query-replace-regexp] . vr/query-replace)))
(use-package anzu
  :diminish anzu-mode
  :bind (([remap query-replace] . anzu-query-replace)
         ;; ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :init (add-hook 'after-init-hook #'global-anzu-mode)
  :config (setq anzu-replace-to-string-separator
                (if (char-displayable-p ?→) " → " " -> ")))

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :init
  ;; show org ediffs unfolded
  (with-eval-after-load 'outline
    (add-hook 'ediff-prepare-buffer-hook #'show-all))
  ;; restore window layout when done
  (with-eval-after-load 'winner
    (add-hook 'ediff-quit-hook #'winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :init (add-hook 'after-init-hook #'electric-pair-mode))

;; Increase selected region by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; ialign
(use-package ialign
  :bind
  (("C-x l" . ialign)))

;; On-the-fly spell checker
(use-package flyspell
  :ensure nil
  :diminish flyspell-mode
  :init (setq flyspell-issue-message-flag nil))

;; Hungry deletion
(use-package hungry-delete
  :diminish hungry-delete-mode
  :init (add-hook 'after-init-hook #'global-hungry-delete-mode)
  )

;; Multiple cursors
;; (use-package multiple-cursors
;;   :bind (("C-S-c C-S-c" . mc/edit-lines)
;;          ("C->" . mc/mark-next-like-this)
;;          ("C-<". mc/mark-previous-like-this)
;;          ("C-S-L" . mc/mark-all-like-this-dwim)
;;          ("s-<mouse-1>" . mc/add-cursor-on-click)
;;          ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; Move to the beginning/end of line or code
(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

;; Treat undo history as a tree
(use-package undo-tree
  :diminish undo-tree-mode
  :init (add-hook 'after-init-hook #'global-undo-tree-mode)
  :config
  (setq
   undo-tree-auto-save-history nil
   undo-tree-history-directory-alist `(("." . ,(concat doom-cache-dir "/undo/")))))

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :diminish subword-mode
  :init (add-hook 'prog-mode-hook #'subword-mode))

(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
