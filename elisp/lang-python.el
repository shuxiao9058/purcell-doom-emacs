;;; package --- python configs
;;; Commentary:
;;; Contains my python configs

(use-package pip-requirements
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

(use-package py-autopep8
  :ensure t)

(use-package auto-virtualenv
  :ensure t
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
)

(use-package elpy
  :ensure t
  :init
  (setq elpy-rpc-backend "jedi")
  (elpy-enable)
  :config
  (add-hook 'python-mode-hook 'elpy-mode)
  (with-eval-after-load 'elpy
  (add-hook 'elpy-mode-hook 'elpy-use-ipython))
  :bind (("M-*" . pop-tag-mark))
  )

(use-package indent-tools
  :ensure t
  :init
  (add-hook 'python-mode-hook
            (lambda () (define-key python-mode-map (kbd "C-c i") 'indent-tools-hydra/body))
            )
  )

(provide 'lang-python)
;;; lang-python.el ends here
