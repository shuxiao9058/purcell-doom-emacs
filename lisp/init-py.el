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
  ;; Activate on changing buffers
  (add-hook 'window-configuration-change-hook 'auto-virtualenv-set-virtualenv)
  ;; Activate on focus in
  (add-hook 'focus-in-hook 'auto-virtualenv-set-virtualenv)
  )

(use-package virtualenvwrapper
  :init
  (venv-initialize-interactive-shells) ;; if you want interactive shell support
  (venv-initialize-eshell) ;; if you want eshell support
  ;; note that setting `venv-location` is not necessary if you
  ;; use the default location (`~/.virtualenvs`), or if the
  ;; the environment variable `WORKON_HOME` points to the right place
  ;; (setq venv-location "/path/to/your/virtualenvs/")
  :config
  (add-hook 'venv-postmkvirtualenv-hook
	    (lambda () (shell-command "pip install rope jedi flake8 pylint")))
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

;; (require-package 'anaconda-eldoc-mode)
;; (require-package 'anaconda-mode)
;; (require-package 'company-anaconda)
;; (add-to-list 'company-backends '(company-anaconda :with company-yasnippet))
;; (add-hook 'python-mode-hook 'anaconda-mode)

(use-package realgud)
(use-package indent-tools
  :ensure t
  :init
  (add-hook 'python-mode-hook
            (lambda () (define-key python-mode-map (kbd "C-c i") 'indent-tools-hydra/body))))

(provide 'init-py)
