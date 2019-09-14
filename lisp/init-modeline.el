;; init-modeline.el --- modeline.	-*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;
;; Modeline
;;

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
               '(:eval (buffer-file-name-truncate t))
               ;; '(:eval (propertize " %b "
               ;;                     'face
               ;;                     (let ((face (buffer-modified-p)))
               ;;                       (if face 'font-lock-warning-face
               ;;                         'font-lock-type-face))
               ;;                     'help-echo (buffer-file-name)))
               " "
               '(:eval (propertize (substring vc-mode 5)
                                   'face 'font-lock-comment-face))

               '(:eval (custom-modeline-flycheck-status))


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

(defun buffer-file-name-truncate (&optional truncate-tail)
  "Propertized `buffer-file-name' that truncates every dir along path.
If TRUNCATE-TAIL is t also truncate the parent directory of the file."
  (let ((dirs (shrink-path-prompt (file-name-directory buffer-file-truename))))
    (if (null dirs)
        (propertize "%b"
                    'face (if (buffer-modified-p) 'font-lock-warning-face 'font-lock-type-face)
                    'help-echo (buffer-file-name))
      (let ((modified-faces (if (buffer-modified-p) 'font-lock-warning-face)))
        (let ((dirname (car dirs))
              (basename (cdr dirs))
              (dir-faces (or modified-faces 'font-lock-type-face))
              (file-faces (or modified-faces 'font-lock-type-face)))
          (concat (propertize (concat dirname
                                      (if truncate-tail (substring basename 0 1) basename)
                                      "/")
                              'face (if dir-faces `(:inherit ,dir-faces)))
                  (propertize (file-name-nondirectory buffer-file-name)
                              'face (if file-faces `(:inherit ,file-faces))
                              'help-echo (buffer-file-name))))))))

(defun custom-modeline-flycheck-status ()
  "Custom status for flycheck with icons."
  (let* ((text (pcase flycheck-last-status-change
                 (`finished (if flycheck-current-errors
                                (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                               (+ (or .warning 0) (or .error 0)))))
                                  (format "%s %s" (insert-icon 'all-the-icons-faicon "bug") count))
                              (format "%s" (insert-icon 'all-the-icons-faicon "check"))))
                 (`running  (format "%s Running" (insert-icon 'all-the-icons-faicon "spinner" -0.15)))
                 (`no-checker  (format "%s No Checker" (insert-icon 'all-the-icons-material "warning" -0.15)))
                 (`not-checked "")
                 (`errored     (format "%s Error" (insert-icon 'all-the-icons-material "warning" -0.15)))
                 (`interrupted (format "%s Interrupted" (insert-icon 'all-the-icons-faicon "stop" -0.15)))
                 (`suspicious  ""))))
    (propertize text
                'help-echo "Show Flycheck Errors"
                'mouse-face '(:box 1)
                'local-map (make-mode-line-mouse-map
                            'mouse-1 (lambda () (interactive) (flycheck-list-errors))))))


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


;; (defun shorten-directory (dir max-length)
;;   "Show up to `max-length' characters of a directory name `dir'."
;;   (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
;;         (output ""))
;;     (when (and path (equal "" (car path)))
;;       (setq path (cdr path)))
;;     (while (and path (< (length output) (- max-length 4)))
;;       (setq output (concat (car path) "/" output))
;;       (setq path (cdr path)))
;;     (when path
;;       (setq output (concat ".../" output)))
;;     output))

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
