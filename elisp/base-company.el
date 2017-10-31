(use-package company
  :ensure t
  :init
  (setq company-minimum-prefix-length 2)
  (setq company-dabbrev-ignore-case t)
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "") 'company-complete-common-or-cycle)

  (global-set-key (kbd "C-c y") 'company-yasnippet)
  )

(defun sanityinc/local-push-company-backend (backend)
  "Add BACKEND to a buffer-local version of `company-backends'."
  (set (make-local-variable 'company-backends)
       (append (list backend) company-backends)))
(require-package 'company-quickhelp)
(company-quickhelp-mode 1)

;; https://www.emacswiki.org/emacs/CompanyMode
;; (defun check-expansion ()
;;   (save-excursion
;;     (if (looking-at "\\_>") t
;;       (backward-char 1)
;;       (if (looking-at "\\.") t
;; 	(backward-char 1)
;; 	(if (looking-at "->") t nil)))))

;; (defun do-yas-expand ()
;;   (let ((yas/fallback-behavior 'return-nil))
;;     (yas/expand)))

;; (defun tab-indent-or-complete ()
;;   (interactive)
;;   (if (minibufferp)
;;       (minibuffer-complete)
;;     (if (or (not yas/minor-mode)
;; 	    (null (do-yas-expand)))
;; 	(if (check-expansion)
;; 	    (company-complete-common-or-cycle)
;; 	  (indent-for-tab-command)))))

;; (global-set-key [tab] 'tab-indent-or-complete)

(provide 'base-company)
