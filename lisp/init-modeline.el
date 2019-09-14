;; init-modeline.el --- modeline.	-*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;
;; Modeline
;;
;; Auto updating version control information
;; (setq auto-revert-check-vc-info t)
;; (setq-default mode-line-format (list
;; ;; ⚿ for locked buffer. ⛯ for modified buffer. ⛆ is the normal one.
;;   '((:eval
;;      (cond
;;       (buffer-read-only
;;        (propertize " ⚿ " 'face '(:foreground "red" :weight 'bold)))
;;       ((buffer-modified-p)
;;        (propertize " ⛯ " 'face '(:foreground "orange")))
;;       ((not (buffer-modified-p))
;;        (propertize " ⛆ " 'face '(:foreground "gray85"))))))
;;   ;; Use all-the-icons to display the icon of current major mode
;;   '(:eval (propertize (all-the-icons-icon-for-mode major-mode
;;           :height (/ all-the-icons-scale-factor 1.4)
;;           :v-adjust -0.03)))
;;   ;; Show the file name with full path
;;   " %f "
;;   ;; Show the current position of the cursor in buffer
;;   'mode-line-position
;;   ;; Show the current major mode name
;;   "[" 'mode-name "] "
;;   ;; Check if the buffer is in any version control system, if yes, show the branch
;;   '(:eval
;;     (if vc-mode
;;         (let* ((noback (replace-regexp-in-string
;;                            (format "^ %s" (vc-backend buffer-file-name)) " " vc-mode))
;;                (face (cond ((string-match "^ -" noback) 'mode-line-vc)
;;                            ((string-match "^ [:@]" noback) 'mode-line-vc-edit)
;;                            ((string-match "^ [!\\?]" noback) 'mode-line-vc-modified))))
;;           (format "[git:%s]" (substring noback 2)))))))(provide 'init-modeline)


(setq-default mode-line-format
              (list
               ;; ⚿ for locked buffer. ⛯ for modified buffer. ⛆ is the normal one.
               '((:eval
                  (cond
                   (buffer-read-only
                    (propertize " ⚿ " 'face '(:foreground "red" :weight 'bold)))
                   ((buffer-modified-p)
                    (propertize " ⛯ " 'face '(:foreground "orange")))
                   ((not (buffer-modified-p))
                    (propertize " ⛆ " 'face '(:foreground "gray85"))))))
               ;; Use all-the-icons to display the icon of current major mode
               '(:eval (propertize (all-the-icons-icon-for-mode major-mode
                                                                :height (/ all-the-icons-scale-factor 1.4)
                                                                :v-adjust -0.03)))

               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize " %b "
                                   'face
                                   (let ((face (buffer-modified-p)))
                                     (if face 'font-lock-warning-face
                                       'font-lock-type-face))
                                   'help-echo (buffer-file-name)))

               '(:eval (propertize (substring vc-mode 5)
                                   'face 'font-lock-comment-face))


               ;; line and column
               " ("
               (propertize "%02l" 'face 'font-lock-keyword-face) ","
               (propertize "%02c" 'face 'font-lock-keyword-face)
               ") "

               ;; relative position, size of file
               " ["
               (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
               "/"
               (propertize "%I" 'face 'font-lock-constant-face) ;; size
               "] "

               (flycheck-mode flycheck-mode-line)


               ;; spaces to align right
               '(:eval (propertize
                        " " 'display
                        `((space :align-to (- (+ right right-fringe right-margin)
                                              ,(+ 6 (string-width (let ((sys (coding-system-plist buffer-file-coding-system)))
                                                                    (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                                                                           "UTF-8")
                                                                          (t (upcase (symbol-name (plist-get sys :name))))))))
                                              ,(+ 3 (string-width mode-name)))))))

               ;; "Displays the encoding and eol style of the buffer the same way Atom does."
               '(:eval
                 (propertize
                  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
                            (0 "  LF ")
                            (1 "CRLF ")
                            (2 "  CR "))
                          (let ((sys (coding-system-plist buffer-file-coding-system)))
                            (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                                   "UTF-8")
                                  (t (upcase (symbol-name (plist-get sys :name))))))
                          " ")))

               ;; the current major mode
               '(:eval
                 (propertize
                  (concat (format-mode-line mode-name)
                          (when (stringp mode-line-process)
                            mode-line-process)
                          (and (featurep 'face-remap)
                               (/= text-scale-mode-amount 0)
                               (format " %+d" text-scale-mode-amount)))
                  'face 'font-lock-string-face))
               ;;minor-mode-alist
               ))


;; (set-face-attribute 'mode-line           nil :background "light blue")
;; (set-face-attribute 'mode-line-buffer-id nil :background "blue" :foreground "white")
;; (defface mode-line-directory
;;   '((t :background "blue" :foreground "gray"))
;;   "Face used for buffer identification parts of the mode line."
;;   :group 'mode-line-faces
;;   :group 'basic-faces)
;; (set-face-attribute 'mode-line-highlight nil :box nil :background "deep sky blue")
;; (set-face-attribute 'mode-line-inactive  nil :inherit 'default)


;; (setq mode-line-position
;;       '(;; %p print percent of buffer above top of window, o Top, Bot or All
;;         ;; (-3 "%p")
;;         ;; %I print the size of the buffer, with kmG etc
;;         ;; (size-indication-mode ("/" (-4 "%I")))
;;         ;; " "
;;         ;; %l print the current line number
;;         ;; %c print the current column
;;         (line-number-mode ("%l" (column-number-mode ":%c")))))


(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; (defvar mode-line-directory
;;   '(:propertize
;;     (:eval (if (buffer-file-name) (concat " " (shorten-directory default-directory 20)) " "))
;;     face mode-line-directory)
;;   "Formats the current directory.")
;; (put 'mode-line-directory 'risky-local-variable t)

;; (setq-default mode-line-buffer-identification
;;               (propertized-buffer-identification "%b "))


;; (setq-default mode-line-format
;;               '("%e"
;;                 mode-line-front-space
;;                 mode-line-mule-info -- I'm always on utf-8
;;                 mode-line-client
;;                 mode-line-modified
;;                 ;; mode-line-remote -- no need to indicate this specially
;;                 ;; mode-line-frame-identification -- this is for text-mode emacs only
;;                 " "
;;                 mode-line-directory
;;                 mode-line-buffer-identification
;;                 " "
;;                 mode-line-position
;;                 ;;(vc-mode vc-mode)  -- I use magit, not vc-mode
;;                 (flycheck-mode flycheck-mode-line)
;;                 " "
;;                 ;; mode-line-modes
;;                 mode-line-misc-info
;;                 mode-line-end-spaces))

(provide 'init-modeline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-modeline.el ends here
