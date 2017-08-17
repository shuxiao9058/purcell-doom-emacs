
(use-package org
  :config
  (setq org-directory "~/org-files"
        org-default-notes-file (concat org-directory "/todo.org"))
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda))

(use-package ob-ipython)
;; (require 'ob-python)
;; plantuml
;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (plantuml . t)
   (ditaa , t)
   (dot . t)
   (emacs-lisp . t)
   (R . t)
   (ipython . t)
   (ruby . t)
   (gnuplot . t)
   (clojure . t)
   (sh . t)
   (ledger . t)
   (org . t)
   (latex . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/extra-bin/bin/plantuml.jar"))
(setq org-ditaa-jar-path (format "%s%s" ""
				 (expand-file-name "~/.emacs.d/extra-bin/bin/ditaa.jar")) )

;; Enable plantuml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

;; Integration with org-mode
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

(add-hook 'org-babel-after-execute-hook 'naso/display-inline-images 'append)

(add-hook 'org-mode-hook '(lambda ()(setq truncate-lines t)) 'append)

(defun naso/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(use-package org-projectile
  :config
  (org-projectile:per-repo)
  (setq org-projectile:per-repo-filename "todo.org"
	org-agenda-files (append org-agenda-files (org-projectile:todo-files))))

(require-package 'org-bullets)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


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
;; ’$’	org-end-of-line
;; ’^’	org-beginning-of-line
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
  :ensure t
  :diminish
  evil-org-mode)


(provide 'base-org)
