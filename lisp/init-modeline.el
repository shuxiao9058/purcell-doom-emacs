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
;; (defun mode-line-fill (face reserve)
;;   "Return empty space using FACE and leaving RESERVE space on the right."
;;   (unless reserve
;;     (setq reserve 20))
;;   (when (and window-system (eq 'right (get-scroll-bar-mode)))
;;     (setq reserve (- reserve 3)))
;;   (propertize " "
;;               'display `((space :align-to
;;                                 (- (+ right right-fringe right-margin) ,reserve)))
;;               'face face))



;; (setq-default mode-line-format
;;               (list
;;                "%1 "
;;                ;; the buffer name; the file name as a tool tip
;;                '(:eval (propertize "%b " 'face (if (buffer-modified-p) '(:background "#d33682" :foreground "#fdf6e3" :weight bold)
;;                                                  '(:background "#268bd2" :foreground "#fdf6e3" :weight normal)))
;;                        'help-echo (buffer-file-name)))
;;               '(:propertize " %p/%I " face (:background "gray60" :foreground "#fdf6e3"))
;;               '(:eval (propertize (concat " " (eyebrowse-mode-line-indicator) " ")))
;;               '(:eval (propertize (format-time-string "%p·%H:%M ") 'help-echo (format-time-string "%F %a") 'face '(:inherit 'font-lock-doc-face)))
;;               '(:propertize vc-mode face (:inherit font-lock-keyword-face :weight bold))
;;               " {%m} " "-%-"
;;               ;; the current major mode for the buffer.
;;               '(:eval (propertize "%m" 'face 'font-lock-string-face
;;                                   'help-echo buffer-file-coding-system))

;;               ;; evil state
;;               '(:eval evil-mode-line-tag)

;;               ;; minor modes
;;               minor-mode-alist
;;               " "
;;               ;; git info
;;               `(vc-mode vc-mode)

;;               " "

;;               ;; global-mode-string goes in mode-line-misc-info
;;               mode-line-misc-info

;;               (mode-line-fill 'mode-line 20)

;;               ;; line and column
;;               "(" ;; '%02' to set to 2 chars at least; prevents flickering
;;               (propertize "%02l" 'face 'font-lock-type-face) ","
;;               (propertize "%02c" 'face 'font-lock-type-face)
;;               ") "

;;               '(:eval (buffer-encoding-abbrev))
;;               mode-line-end-spaces
;;               )
;; )


(provide 'init-modeline)
