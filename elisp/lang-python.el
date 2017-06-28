;;; package --- python configs
;;; Commentary:
;;; Contains my python configs

;;; Code:
;; (setq auto-mode-alist
;;       (append '(("SConstruct\\'" . python-mode)
;; 		("SConscript\\'" . python-mode))
;;               auto-mode-alist))

(use-package pip-requirements
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

;; (when (maybe-require-package 'anaconda-mode)
;;   (after-load 'python
;;     (add-hook 'python-mode-hook 'anaconda-mode)
;;     (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;;     (add-hook 'python-mode-hook 'flycheck-mode)
;;     (when (executable-find "ipython")
;;       (setq python-shell-interpreter "ipython"))

;;     (setq
;;      python-shell-interpreter-args "-i C:\\Python27\\Scripts\\ipython-script.py --pylab=qt"
;;      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;      python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
;;      python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
;;      python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;;     (when (maybe-require-package 'company-anaconda)
;;       (after-load 'company
;; 	(add-hook 'python-mode-hook
;; 		  (lambda () (sanityinc/local-push-company-backend 'company-anaconda)))))))

(use-package py-autopep8
  :ensure t)
(use-package python
  :mode ("\\.py" . python-mode)
  :config
  (use-package elpy
    :init
    (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
    :config
    (setq elpy-rpc-backend "jedi")
    (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
    :bind (:map elpy-mode-map
		("M-." . elpy-goto-definition)
		("M-," . pop-tag-mark)))
  ;; (setq elpy-rpc-python-command "python3")
  (setq
   python-shell-interpreter-args "-i C:\\Python27\\Scripts\\ipython-script.py --pylab=qt"
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
  (elpy-enable)
  (elpy-use-ipython))




;; (use-package pyenv-mode
;;   :init
;;   (add-to-list 'exec-path "~/.pyenv/shims")
;;   (setenv "WORKON_HOME" "~/.pyenv/versions/")
;;   :config
;;   (pyenv-mode)
;;   :bind
;;   ("C-x p e" . pyenv-activate-current-project))

;; (defun pyenv-init()
;;   (setq global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global")))
;;   (message (concat "Setting pyenv version to " global-pyenv))
;;   (pyenv-mode-set global-pyenv)
;;   (defvar pyenv-current-version nil global-pyenv))

;; (defun pyenv-activate-current-project ()
;;   "Automatically activates pyenv version if .python-version file exists."
;;   (interactive)
;;   (f-traverse-upwards
;;    (lambda (path)
;;      (message path)
;;      (let ((pyenv-version-path (f-expand ".python-version" path)))
;;        (if (f-exists? pyenv-version-path)
;;           (progn
;;             (setq pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8)))
;;             (pyenv-mode-set pyenv-current-version)
;;             (pyvenv-workon pyenv-current-version)
;;             (message (concat "Setting virtualenv to " pyenv-current-version))))))))

;; (add-hook 'after-init-hook 'pyenv-init)
;; (add-hook 'projectile-after-switch-project-hook 'pyenv-activate-current-project)

(provide 'lang-python)
;;; lang-python.el ends here
