;;; init-org.el
;;; Code:

(use-package org
  :config
  (setq org-directory "~/org-files"
        org-default-notes-file (concat org-directory "/todo.org"))
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda))

(use-package ob-ipython
  :init
  (with-eval-after-load 'company
    (make-local-variable 'company-backend)
    (cl-pushnew 'company-ob-ipython company-backends)))

;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (plantuml . t)
   (ditaa , t)
   (dot . t)
   (emacs-lisp . t)
   (ipython . t)
   (ruby . t)
   (gnuplot . t)
   ;(sh . t)
   (org . t)
   (latex . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
; (setq org-plantuml-jar-path
      ; (expand-file-name "~/forwin/bin/plantuml.jar"))
; (setq org-ditaa-jar-path (format "%s%s" ""
                                 ; (expand-file-name "~/forwin/bin/ditaa.jar")) )

(add-hook 'org-babel-after-execute-hook 'naso/display-inline-images 'append)
(add-hook 'org-mode-hook '(lambda ()(setq truncate-lines t)) 'append)
(defun naso/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(use-package plantuml-mode
  :init
  (setq plantuml-jar-path (expand-file-name sea-etc-dir "plantuml.jar"))
  ;; Enable plantuml-mode for PlantUML files
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  ;; Integration with org-mode
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

(use-package org-projectile
  :config
  (org-projectile:per-repo)
  (setq org-projectile:per-repo-filename "todo.org"
        org-agenda-files (append org-agenda-files (org-projectile:todo-files))))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; TODO: fail gracefully
(defun sanityinc/grab-ditaa (url jar-name)
  "Download URL and extract JAR-NAME as `org-ditaa-jar-path'."
  ;; TODO: handle errors
  (message "Grabbing " jar-name " for org.")
  (let ((zip-temp (make-temp-name "emacs-ditaa")))
    (unwind-protect
        (progn
          (when (executable-find "unzip")
            (url-copy-file url zip-temp)
            (shell-command (concat "unzip -p " (shell-quote-argument zip-temp)
                                   " " (shell-quote-argument jar-name) " > "
                                   (shell-quote-argument org-ditaa-jar-path)))))
      (when (file-exists-p zip-temp)
        (delete-file zip-temp)))))
		
; (after! ob-ditaa
  ; (unless (and (boundp 'org-ditaa-jar-path)
               ; (file-exists-p org-ditaa-jar-path))
    ; (let ((jar-name "ditaa0_9.jar")
          ; (url "http://jaist.dl.sourceforge.net/project/ditaa/ditaa/0.9/ditaa0_9.zip"))
      ; (setq org-ditaa-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
      ; (unless (file-exists-p org-ditaa-jar-path)
        ; (sanityinc/grab-ditaa url jar-name)))))

; (after! ob-plantuml
  ; (let ((jar-name "plantuml.jar")
        ; (url "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar"))
    ; (setq org-plantuml-jar-path (expand-file-name jar-name sea-etc-dir))
    ; (unless (file-exists-p org-plantuml-jar-path)
      ; (url-copy-file url org-plantuml-jar-path))))


(provide 'init-org)
