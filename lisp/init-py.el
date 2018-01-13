;;; package --- python configs
;;; Commentary:
;;; Contains my python configs

(use-package elpy
  :ensure t
  :init
  (setq elpy-rpc-backend "jedi")
  (add-hook 'python-mode-hook 'elpy-mode)
  :config
  (with-eval-after-load 'elpy
    (elpy-use-ipython "ipython")
    (when (require 'flycheck nil t)
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (add-hook 'elpy-mode-hook 'flycheck-mode))
    (use-package py-autopep8
      :init
      (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)))
  :bind (("M-*" . pop-tag-mark))
  )
;; (use-package elpy
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'python-mode-hook 'elpy-mode)
;;   :bind (("M-*" . pop-tag-mark))
;;   :config
;;   (progn
;;     ;; Use Flycheck instead of Flymake
;;     (when (require 'flycheck nil t)
;;       (remove-hook 'elpy-modules 'elpy-module-flymake)
;;       (remove-hook 'elpy-modules 'elpy-module-yasnippet)
;;       (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
;;       (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;       (add-hook 'elpy-mode-hook 'flycheck-mode))
;;     (setq elpy-modules (delq 'elpy-module-company elpy-modules))
;;     ;; elpy uses rope by defaul for autocompletion.
;;     ;; You can permanently prefer jedi by setting this
;;     (setq elpy-rpc-backend "jedi")
;;     ;;--------------------------------------------------------------------------
;; ;;;  comment/uncoment the block to consider python or ipython
;;     ;;--------------------------------------------------------------------------
;;     (if (executable-find "ipython3")
;;         (elpy-use-ipython "ipython3")
;;       (if (executable-find "ipython")
;;           (elpy-use-ipython "ipython")))
;;     ;;--------------------------------------------------------------------------
;;     ;;(elpy-use-cpython)
;;     (elpy-enable)
;;     (add-to-list 'company-backends
;;                  (company-backend-with-yas 'elpy-company-backend))))
;; (use-package company-jedi
;;   :ensure t
;;   :defer t
;;   :after python
;;   :config
;;   (add-to-list 'company-backends
;;                (company-backend-with-yas 'company-jedi))
;;   )
;; (when (executable-find "ipython")
;;   (setq python-shell-interpreter "ipython")
;;   (setq python-shell-interpreter-args "--pylab"))
;; (require-package 'anaconda-eldoc-mode)
;; (require-package 'anaconda-mode)
;; (require-package 'company-anaconda)
;; (add-to-list 'company-backends '(company-anaconda :with company-yasnippet))
;; (add-hook 'python-mode-hook 'anaconda-mode)

(use-package realgud
  :ensure t)
(use-package indent-tools
  :ensure t
  :init
  (add-hook 'python-mode-hook 'indent-tools-minor-mode)
  :bind
  ("C-c i" . indent-tools-hydra/body))


(provide 'init-py)
;;; init-py.el ends here
