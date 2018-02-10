;;; init-modeline.el
;; Author: Haibo Wang <nasoundead@163.com>
;; Version: 0.0.1
;; URL: https://github.com/nasoundead/.emacs.d
;; Keywords:
;; Compatibility:
;; Reference:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Package configurations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(require 'all-the-icons)
(setq evil-normal-state-tag   (propertize "NORMAL" 'face '((:background "DarkGoldenrod2" :foreground "black" :weight bold)))
      evil-emacs-state-tag    (propertize "EMACS " 'face '((:background "SkyBlue2" :foreground "black" :weight bold)))
      evil-insert-state-tag   (propertize "INSERT" 'face '((:background "chartreuse3") :foreground "white" :weight bold))
      evil-motion-state-tag   (propertize "MOTION" 'face '((:background "plum3") :foreground "white" :weight bold))
      evil-visual-state-tag   (propertize "VISUAL" 'face '((:background "gray" :foreground "black" :weight bold)))
      evil-operator-state-tag (propertize "OPERAT" 'face '((:background "purple" :weight bold))))

;; (defun custom-modeline-window-number ()
;;   (propertize (format " %s" (window-numbering-get-number))
;;               'display '(raise 0.0)))

;; (defun custom-modeline-modified ()
;;   (propertize " %b " 'face (if (buffer-modified-p)
;;                                '(:background "#d33682" :foreground "#fdf6e3" :weight bold)
;;                              '(:background "#268bd2" :foreground "#fdf6e3" :weight normal))
;;               'help-echo (buffer-file-name)))

;; (defun custom-modeline-region-info ()
;;   (when mark-active
;;     (let ((words (count-lines (region-beginning) (region-end)))
;;           (chars (count-words (region-end) (region-beginning))))
;;       (concat
;;        (propertize (format "   %s" (all-the-icons-octicon "pencil") words chars)
;;                    'face `(:family ,(all-the-icons-octicon-family))
;;                    'display '(raise -0.0))
;;        (propertize (format " (%s, %s)" words chars)
;;                    'face `(:height 0.9))))))

;; (defun -custom-modeline-github-vc ()
;;   (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
;;     (concat
;;      (propertize (format " %s" (all-the-icons-alltheicon "git")) 'face `(:height 1.2) 'display '(raise -0.1))
;;      " · "
;;      (propertize (format "%s" (all-the-icons-octicon "git-branch"))
;;                  'face `(:height 1.3 :family ,(all-the-icons-octicon-family))
;;                  'display '(raise -0.1))
;;      (propertize (format " %s" branch) 'face `(:height 0.9)))))

;; (defun -custom-modeline-svn-vc ()
;;   (let ((revision (cadr (split-string vc-mode "-"))))
;;     (concat
;;      (propertize (format " %s" (all-the-icons-faicon "cloud")) 'face `(:height 1.2) 'display '(raise -0.1))
;;      (propertize (format " · %s" revision) 'face `(:height 0.9)))))

;; (defun custom-modeline-icon-vc ()
;;   (when vc-mode
;;     (cond
;;      ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
;;      ((string-match "SVN-" vc-mode) (-custom-modeline-svn-vc))
;;      (t (format "%s" vc-mode)))))

;; (defun custom-modeline-flycheck-status ()
;;   (let* ((text (pcase flycheck-last-status-change
;;                  (`finished (if flycheck-current-errors
;;                                 (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
;;                                                (+ (or .warning 0) (or .error 0)))))
;;                                   (format "* %s Issue%s" count (unless (eq 1 count) "s")))
;;                               "o No Issues"))
;;                  (`running     "- Running")
;;                  (`no-checker  "! No Checker")
;;                  (`not-checked "! Disabled")
;;                  (`errored     "! Error")
;;                  (`interrupted "! Interrupted")
;;                  (`suspicious  ""))))
;;     (propertize text
;;                 'help-echo "Show Flycheck Errors"
;;                 'mouse-face '(:box 1)
;;                 'local-map (make-mode-line-mouse-map
;;                             'mouse-1 (lambda () (interactive) (flycheck-list-errors))))))

;; (defun custom-modeline-time ()
;;   (let* ((hour (string-to-number (format-time-string "%I")))
;;          (icon (all-the-icons-wicon (format "time-%s" hour) :height 1.3 :v-adjust 0.0)))
;;     (concat
;;      (propertize (format-time-string " %H:%M ") 'face `(:height 0.9))
;;      (propertize (format "%s " icon) 'face `(:height 1.0 :family ,(all-the-icons-wicon-family)) 'display '(raise -0.0)))))

;; (setq mode-line-format (list
;;                         '(:eval (custom-modeline-window-number))
;;                         '(:eval evil-mode-line-tag)
;;                         '(:eval (custom-modeline-modified))
;;                         mode-line-modes
;;                         mode-line-misc-info
;;                         '(:eval (custom-modeline-icon-vc))
;;                         '(:eval (custom-modeline-region-info))
;;                         '(:eval (custom-modeline-flycheck-status))
;;                         '(:eval (custom-modeline-time))
;;                         ))
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
                                  'face face
                                  'mouse-face '(:box 1)
                                  'local-map (make-mode-line-mouse-map
                                              'mouse-1 (lambda () (interactive) (flycheck-list-errors)))

                                  )))
                   (`interrupted " -")
                   (`suspicious '(propertize " ?" 'face 'warning))))
               '(:eval (propertize (buffer-encoding-abbrev)
                                   'help-echo (format "%s" buffer-file-coding-system)))
               "(" ;; '%02' to set to 2 chars at least; prevents flickering
               (propertize "%03l")
               ","
               (propertize "%02c")
               ")"
               '(:propertize " %p/%I")
               '(:eval (propertize (concat " " (eyebrowse-mode-line-indicator))))
               '(:propertize vc-mode face (:inherit font-lock-keyword-face :weight bold))
               mode-line-modes
               mode-line-misc-info
               '(:eval (propertize (format-time-string " %H:%M") 'help-echo (format-time-string "%F %a")))
               mode-line-end-spaces)
              )

(provide 'init-modeline)
