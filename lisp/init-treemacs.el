;; Treemacs
(use-package treemacs
  :ensure t
  :config
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-desc
        treemacs-persist-file (concat sea-cache-dir "treemacs-persist")
        treemacs-last-error-persist-file (concat sea-cache-dir "treemacs-last-error-persist"))
  (after! treemacs-persistence
    ;; This variable is defined with defconst, so we must wait to change it until
    ;; it has loaded.
    (setq treemacs--last-error-persist-file
          (concat sea-cache-dir
                  "treemacs-persist-at-last-error")))
  (after! treemacs
    (set-popup-rule! "^ \\*Treemacs"
                     :side treemacs-position
                     :size treemacs-width
                     :quit nil
                     :ttl 0)

    ;; Don't follow the cursor
    (treemacs-follow-mode -1)

    (after! ace-window
      (delq! 'treemacs-mode aw-ignored-buffers)))
  ;; Improve treemacs icons
  (with-eval-after-load 'treemacs
    (with-eval-after-load 'all-the-icons
      (let ((all-the-icons-default-adjust 0)
            (tab-width 1))
        ;; Root icon
        (setq treemacs-icon-root-png
              (concat (all-the-icons-octicon "repo" :height 0.8 :v-adjust -0.2)  " "))
        ;; File icons
        (setq treemacs-icon-open-png
              (concat
               (all-the-icons-octicon "chevron-down" :height 0.8 :v-adjust 0.1)
               "\t"
               (all-the-icons-octicon "file-directory" :v-adjust 0)
               "\t")
              treemacs-icon-closed-png
              (concat
               (all-the-icons-octicon "chevron-right" :height 0.8
                                      :v-adjust 0.1 :face 'font-lock-doc-face)
               "\t"
               (all-the-icons-octicon "file-directory" :v-adjust 0 :face 'font-lock-doc-face)
               "\t"))
        ;; File type icons
        (setq treemacs-icons-hash (make-hash-table :size 200 :test #'equal)
              treemacs-icon-fallback (concat
                                      "\t\t"
                                      (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver
                                                            :height 0.8 :v-adjust 0.0)
                                      "\t")
              treemacs-icon-text treemacs-icon-fallback)

        (dolist (item all-the-icons-icon-alist)
          (let* ((extension (car item))
                 (func (cadr item))
                 (args (append (list (caddr item)) '(:v-adjust -0.05) (cdddr item)))
                 (icon (apply func args))
                 (key (s-replace-all '(("^" . "") ("\\" . "") ("$" . "") ("." . "")) extension))
                 (value (concat "\t\t" icon "\t")))
            (unless (ht-get treemacs-icons-hash (s-replace-regexp "\\?" "" key))
              (ht-set! treemacs-icons-hash (s-replace-regexp "\\?" "" key) value))
            (unless (ht-get treemacs-icons-hash (s-replace-regexp ".\\?" "" key))
              (ht-set! treemacs-icons-hash (s-replace-regexp ".\\?" "" key) value))))))))

(use-package treemacs-projectile
  :ensure t
  :after treemacs)

(use-package treemacs-evil
  :after treemacs
  :config
  (define-key! evil-treemacs-state-map
    [return] #'treemacs-RET-action
    [tab]    #'treemacs-TAB-action
    "TAB"    #'treemacs-TAB-action))
(use-package treemacs-magit
  :after treemacs magit)

(provide 'init-treemacs)
;;; init-treemacs.el ends here
