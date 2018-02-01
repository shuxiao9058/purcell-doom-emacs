;;init-modeline.el
(setq evil-normal-state-tag   (propertize "NORMAL"
                                          'face '((:background "DarkGoldenrod2" :foreground "black" :weight bold)))
      evil-emacs-state-tag    (propertize "EMACS "
                                          'face '((:background "SkyBlue2" :foreground "black" :weight bold)))
      evil-insert-state-tag   (propertize "INSERT"
                                          'face '((:background "chartreuse3") :foreground "white" :weight bold))
      evil-motion-state-tag   (propertize "MOTION"
                                          'face '((:background "plum3") :foreground "white" :weight bold))
      evil-visual-state-tag   (propertize "VISUAL"
                                          'face '((:background "gray" :foreground "black" :weight bold)))
      evil-operator-state-tag (propertize "OPERAT"
                                          'face '((:background "purple" :weight bold))))

(defun buffer-encoding-abbrev ()
  "The line ending convention used in the buffer."
  (let ((buf-coding (format "%s" buffer-file-coding-system)))
    (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
        (match-string 1 buf-coding)
      buf-coding)))
(setq-default mode-line-format
              (list
               "%1"
               '(:eval evil-mode-line-tag)
               '(:eval (propertize " %b " 'face (if (buffer-modified-p)
                                                    '(:background "#d33682" :foreground "#fdf6e3" :weight bold)
                                                  '(:background "#268bd2" :foreground "#fdf6e3" :weight normal))
                                   'help-echo (buffer-file-name)))
               '(:eval
                 (pcase flycheck-last-status-change
                   (`not-checked nil)
                   (`no-checker (propertize "-" 'face 'warning))
                   (`running (propertize "*" 'face 'success))
                   (`errored (propertize "!" 'face 'error))
                   (`finished
                    (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                           (no-errors (cdr (assq 'error error-counts)))
                           (no-warnings (cdr (assq 'warning error-counts)))
                           (face (cond (no-errors 'error)
                                       (no-warnings 'warning)
                                       (t 'success))))
                      (propertize (format "[%s/%s]" (or no-errors 0) (or no-warnings 0))
                                  'face face)))
                   (`interrupted " -")
                   (`suspicious '(propertize " ?" 'face 'warning))))
               '(:eval (propertize (buffer-encoding-abbrev)
                                   'help-echo (format "%s" buffer-file-coding-system)))
               " L" ;; '%02' to set to 2 chars at least; prevents flickering
               (propertize "%l")
               '(:propertize " %p/%I")
               '(:eval (propertize (concat " " (eyebrowse-mode-line-indicator))))
               '(:eval (propertize (format-time-string " %p·%H:%M ") 'help-echo (format-time-string "%F %a")))
               '(:propertize vc-mode face (:inherit font-lock-keyword-face :weight bold))
               mode-line-modes
               mode-line-misc-info
               mode-line-end-spaces)
              )

;; (use-package telephone-line)
;; (require 'telephone-line-config)
;; (telephone-line-evil-config)

(provide 'init-modeline)
