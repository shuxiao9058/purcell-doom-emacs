
(setq tab-always-indent 'complete)  ;; use 't when company is disabled
(add-to-list 'completion-styles 'initials t)
;; Stop completion-at-point from popping up completion buffers so eagerly
(setq completion-cycle-threshold 5)


(when (maybe-require-package 'company)
  (add-hook 'after-init-hook 'global-company-mode)
  (after-load 'company
    (diminish 'company-mode "CMP")
    (define-key company-mode-map (kbd "M-/") 'company-complete)
    (define-key company-active-map (kbd "M-/") 'company-select-next)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (setq-default company-backends '((company-capf company-dabbrev-code) company-dabbrev)
                  company-dabbrev-other-buffers 'all))
  (global-set-key (kbd "M-C-/") 'company-complete)
  ;;makes completion start automatically rather than waiting for 3 chars / 0.5sec
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0)
  (when (maybe-require-package 'company-quickhelp)
    (add-hook 'after-init-hook 'company-quickhelp-mode))

  (defun sanityinc/local-push-company-backend (backend)
    "Add BACKEND to a buffer-local version of `company-backends'."
    (set (make-local-variable 'company-backends)
         (append (list backend) company-backends))))

(require-package 'company-quickhelp)
(company-quickhelp-mode 1)

(provide 'base-company)
