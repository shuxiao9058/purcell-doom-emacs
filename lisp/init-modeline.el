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
;; (require 'all-the-icons)
;; (setq evil-normal-state-tag   (propertize "NORMAL" 'face '((:background "DarkGoldenrod2" :foreground "black" :weight bold)))
;;       evil-emacs-state-tag    (propertize "EMACS " 'face '((:background "SkyBlue2" :foreground "black" :weight bold)))
;;       evil-insert-state-tag   (propertize "INSERT" 'face '((:background "chartreuse3") :foreground "white" :weight bold))
;;       evil-motion-state-tag   (propertize "MOTION" 'face '((:background "plum3") :foreground "white" :weight bold))
;;       evil-visual-state-tag   (propertize "VISUAL" 'face '((:background "gray" :foreground "black" :weight bold)))
;;       evil-operator-state-tag (propertize "OPERAT" 'face '((:background "purple" :weight bold))))

;; (defun buffer-encoding-abbrev ()
;;   "The line ending convention used in the buffer."
;;   (let ((buf-coding (format "%s" buffer-file-coding-system)))
;;     (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
;;         (match-string 1 buf-coding)
;;       buf-coding)))
;; (setq-default mode-line-format
;;               (list
;;                "%1"
;;                '(:eval evil-mode-line-tag)
;;                '(:eval (propertize " %b " 'face (if (buffer-modified-p)
;;                                                     '(:background "#d33682"
;;                                                                   :foreground "#fdf6e3"
;;                                                                   :weight bold)
;;                                                   nil
;;                                                   ;; '(:background "#268bd2" :foreground "#fdf6e3" :weight normal)
;;                                                   )
;;                                    'help-echo (buffer-file-name)))
;;                '(:eval
;;                  (pcase flycheck-last-status-change
;;                    (`not-checked nil)
;;                    (`no-checker (propertize "-" 'face 'warning))
;;                    (`running (propertize "*" 'face 'success))
;;                    (`errored (propertize "!" 'face 'error))
;;                    (`finished
;;                     (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
;;                            (no-errors (cdr (assq 'error error-counts)))
;;                            (no-warnings (cdr (assq 'warning error-counts)))
;;                            (face (cond (no-errors 'error)
;;                                        (no-warnings 'warning)
;;                                        (t 'success))))
;;                       (propertize (format "[%s/%s]" (or no-errors 0) (or no-warnings 0))
;;                                   'face face
;;                                   'mouse-face '(:box 1)
;;                                   'local-map (make-mode-line-mouse-map
;;                                               'mouse-1 (lambda () (interactive) (flycheck-list-errors)))

;;                                   )))
;;                    (`interrupted " -")
;;                    (`suspicious '(propertize " ?" 'face 'warning))))
;;                '(:eval (propertize (buffer-encoding-abbrev)
;;                                    'help-echo (format "%s" buffer-file-coding-system)))
;;                "(" ;; '%02' to set to 2 chars at least; prevents flickering
;;                (propertize "%02l")
;;                ","
;;                (propertize "%02c")
;;                ")"
;;                '(:propertize "%p/%I")
;;                '(:eval (propertize (concat " " (eyebrowse-mode-line-indicator))))
;;                '(:propertize vc-mode face (:inherit font-lock-keyword-face :weight bold))
;;                mode-line-modes
;;                mode-line-misc-info
;;                '(:eval (propertize (format-time-string "%H:%M") 'help-echo (format-time-string "%F %a")))
;;                mode-line-end-spaces)
;;               )

;; (use-package powerline
;;   :init
;;   (powerline-center-theme))
(use-package spaceline
  :init
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (set 'spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  )
;; (use-package spaceline-all-the-icons
;;   :after spaceline
;;   :init
;;   (spaceline-all-the-icons-theme)
;;   (spaceline-all-the-icons--setup-anzu)          ;; Enable anzu searching
;;   (spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
;;   (spaceline-all-the-icons--setup-git-ahead) ;; Enable # of commits ahead of upstream in git
;;   (spaceline-all-the-icons--setup-paradox)   ;; Enable Paradox mode line
;;   (spaceline-all-the-icons--setup-neotree)   ;; Enable Neotree mode line
;;   )
(provide 'init-modeline)
