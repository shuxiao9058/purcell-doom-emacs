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
      (setq elpy-modules (delq 'elpy-module-company elpy-modules))
      (add-hook 'elpy-mode-hook 'flycheck-mode))
    (use-package py-autopep8
      :init
      (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)))
  :bind (("M-*" . pop-tag-mark))
  )

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
