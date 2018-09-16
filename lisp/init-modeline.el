;; init-modeline.el --- modeline.	-*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defun custom-modeline-mode-icon ()
  (format " %s"
    (propertize icon
                'help-echo (format "Major-mode: `%s`" major-mode)
                'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))

(defun custom-modeline-region-info ()
  (when mark-active
    (let ((words (count-lines (region-beginning) (region-end)))
          (chars (count-words (region-end) (region-beginning))))
      (concat
       (propertize (format "   %s" (all-the-icons-octicon "pencil") words chars)
                   'face `(:family ,(all-the-icons-octicon-family))
                   'display '(raise -0.0))
       (propertize (format " (%s, %s)" words chars)
                   'face `(:height 0.9))))))
				   
(defun -custom-modeline-github-vc ()
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (propertize (format " %s" (all-the-icons-alltheicon "git")) 'face `(:height 1.2) 'display '(raise -0.1))
     " · "
     (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                 'face `(:height 1.3 :family ,(all-the-icons-octicon-family))
                 'display '(raise -0.1))
     (propertize (format " %s" branch) 'face `(:height 0.9)))))

(defun -custom-modeline-svn-vc ()
  (let ((revision (cadr (split-string vc-mode "-"))))
    (concat
     (propertize (format " %s" (all-the-icons-faicon "cloud")) 'face `(:height 1.2) 'display '(raise -0.1))
     (propertize (format " · %s" revision) 'face `(:height 0.9)))))

(defun custom-modeline-icon-vc ()
  (when vc-mode
    (cond
      ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
      ((string-match "SVN-" vc-mode) (-custom-modeline-svn-vc))
      (t (format "%s" vc-mode)))))
	  
(defun custom-modeline-flycheck-status ()
  (let* ((text (pcase flycheck-last-status-change
                (`finished (if flycheck-current-errors
                               (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                              (+ (or .warning 0) (or .error 0)))))
                                 (format "✖ %s Issue%s" count (unless (eq 1 count) "s")))
                             "✔ No Issues"))
                (`running     "⟲ Running")
                (`no-checker  "⚠ No Checker")
                (`not-checked "✖ Disabled")
                (`errored     "⚠ Error")
                (`interrupted "⛔ Interrupted")
                (`suspicious  ""))))
     (propertize text
                 'help-echo "Show Flycheck Errors"
                 'mouse-face '(:box 1)
                 'local-map (make-mode-line-mouse-map
                             'mouse-1 (lambda () (interactive) (flycheck-list-errors))))))
(setq-default mode-line-format '("%e" (:eval 
  (concat
    ; (custom-modeline-mode-icon)
    (custom-modeline-icon-vc)
    (custom-modeline-region-info)
    (custom-modeline-flycheck-status)))))
	
(provide 'init-modeline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-modeline.el ends here
