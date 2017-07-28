
;; (setq tab-always-indent 'complete)
;; (add-to-list 'completion-styles 'initials t)
;; (setq completion-cycle-threshold 5)


;; (when (maybe-require-package 'company)
;;   (add-hook 'after-init-hook 'global-company-mode)
;;   (after-load 'company
;;     (diminish 'company-mode "CMP")
;;     (define-key company-active-map (kbd "C-n") 'company-select-next)
;;     (define-key company-active-map (kbd "C-p") 'company-select-previous)
;;     (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
;;     (setq-default company-backends '((company-capf company-dabbrev-code) company-dabbrev)
;;                   company-dabbrev-other-buffers 'all))
;;   (setq company-minimum-prefix-length 1)
;;   (setq company-idle-delay 0)
;;   (when (maybe-require-package 'company-quickhelp)
;;     (add-hook 'after-init-hook 'company-quickhelp-mode))

;;   (defun sanityinc/local-push-company-backend (backend)
;;     "Add BACKEND to a buffer-local version of `company-backends'."
;;     (set (make-local-variable 'company-backends)
;;          (append (list backend) company-backends))))
(use-package company
  :ensure t
  :init
  (setq company-minimum-prefix-length 2)
  (setq company-dabbrev-ignore-case t)
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-s") #'company-filter-candidates)

  (global-set-key (kbd "C-c y") 'company-yasnippet)
  )
(defun sanityinc/local-push-company-backend (backend)
  "Add BACKEND to a buffer-local version of `company-backends'."
  (set (make-local-variable 'company-backends)
       (append (list backend) company-backends)))
(require-package 'company-quickhelp)
(company-quickhelp-mode 1)

(provide 'base-company)
