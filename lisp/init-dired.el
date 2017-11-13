(use-package dired+
  :init
  (setq diredp-hide-details-initially-flag nil)
  :config
  (diredp-toggle-find-file-reuse-dir 1))


;; Open/close directories with double-click, RET or Space keys.
;; To jump to the parent directory, hit the Backspace key.
;; To toggle open/closed state of the subtree of the current directory, hit the x key.
;; RET on different files starts the Ediff (or open file if one absent or the same)
;; Space show the simple diff window for the current file instead of Ediff (or view file if one absent or the same)
;; TAB to fast switch between panels
;; h key to toggle show/hide identical files/directories
;; H key to toggle show/hide hidden/ignored files/directories
;; C key to copy current file or directory to the left or right panel
;; D key to delete current file or directory
;; v key to quick view the current file
;; r initiates the rescan/refresh of current file or subdirectory
;; F5 forces the full rescan.
(use-package ztree
  :defer t
  :ensure t)

(defun vinegar/dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
        (progn
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          (message "h")
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
             (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(defun vinegar/back-to-top ()
  "Move to first file"
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 1))

(defun vinegar/jump-to-bottom ()
  "Move to last file"
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(defun vinegar/move-up ()
  "Move to previous file"
  (interactive)
  (dired-previous-line 1)
  (if (bobp)
      (dired-next-line 1)))

(defun vinegar/move-down ()
  "Move to next file"
  (interactive)
  (dired-next-line 1)
  (if (eobp)
      (dired-next-line -1)))

(defun vinegar/up-directory (&optional other-window)
  "Run Dired on parent directory of current directory."
  (interactive "P")
  (let* ((dir (dired-current-directory))
         (orig (current-buffer))
         (up (file-name-directory (directory-file-name dir))))
    (or (dired-goto-file (directory-file-name dir))
        ;; Only try dired-goto-subdir if buffer has more than one dir.
        (and (cdr dired-subdir-alist)
             (dired-goto-subdir up))
        (progn
          (kill-buffer orig)
          (dired up)
          (dired-goto-file dir)))))

(defun vinegar/dired-diff ()
  "Ediff marked files in dired or selected files in separate window"
  (interactive)
  (let* ((marked-files (dired-get-marked-files nil nil))
     (other-win (get-window-with-predicate
             (lambda (window)
               (with-current-buffer (window-buffer window)
             (and (not (eq window (selected-window)))
                  (eq major-mode 'dired-mode))))))
     (other-marked-files (and other-win
                  (with-current-buffer (window-buffer other-win)
                    (dired-get-marked-files nil)))))
    (cond ((= (length marked-files) 2)
       (if (and (file-directory-p (nth 0 marked-files))
            (file-directory-p (nth 1 marked-files)))
           (ztree-diff (nth 0 marked-files)
               (nth 1 marked-files))
         (ediff-files (nth 0 marked-files)
              (nth 1 marked-files))))
          ((= (length marked-files) 3)
           (ediff-files3 (nth 0 marked-files)
                         (nth 1 marked-files)
                         (nth 2 marked-files)
                         ))
          ((and (= (length marked-files) 1)
                (= (length other-marked-files) 1))
       (if (and (file-directory-p (nth 0 marked-files))
            (file-directory-p (nth 0 other-marked-files)))
           (ztree-diff (nth 0 marked-files)
               (nth 0 other-marked-files)))
           (ediff-files (nth 0 marked-files)
                        (nth 0 other-marked-files)))
          ((= (length marked-files) 1)
           (dired-diff))
          (t (error "mark exactly 2 files, at least 1 locally")))))


(defun vinegar/dired-setup ()
  "Setup custom dired settings for vinegar"
  (setq dired-omit-verbose nil)
  (make-local-variable 'dired-hide-symlink-targets)
  (setq dired-hide-details-hide-symlink-targets nil)

  ;; hide details by default
  (dired-hide-details-mode t)
  ;; omit the .. in dired
  (dired-omit-mode t)

  ;; allow selection with mouse
  (make-local-variable 'mouse-1-click-follows-link)
  (setq mouse-1-click-follows-link nil)

  (local-set-key (kbd  "<mouse-1>") 'vinegar/dired-mouse-click)
  (local-set-key (kbd  "<mouse-3>") 'vinegar/up-directory)
  (local-set-key (kbd  "<down-mouse-3>") nil)
  )

(defun vinegar/dired-mouse-click (event)
  "In Dired, visit the file or directory name you click on."
  (interactive "e")
  (let (window pos file)
    (save-excursion
      (setq window (posn-window (event-end event))
            pos (posn-point (event-end event)))
      (if (not (windowp window))
          (error "No file chosen"))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (setq file (dired-get-file-for-visit)))
    (find-alternate-file file)))

(defun vinegar/dired-mouse-click-3 (event)
  "In Dired, show context menu or go up a directory."
  (interactive "e")
  (let (window pos file)
    (save-excursion
      (setq window (posn-window (event-end event))
            pos (posn-point (event-end event)))
      (if (not (windowp window))
          (error "No file chosen"))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (condition-case-unless-debug ex
          (progn
            (setq file (dired-get-file-for-visit))
            (dired-find-file-other-window))
        ('error
         (vinegar/up-directory)
         ))
      )))

(evil-define-key 'normal  dired-mode-map (kbd "j") 'vinegar/move-down)
(evil-define-key 'normal  dired-mode-map (kbd "k") 'vinegar/move-up)
(evil-define-key 'normal  dired-mode-map (kbd "-") 'vinegar/up-directory)
(evil-define-key 'normal  dired-mode-map (kbd "=") 'vinegar/dired-diff)
(evil-define-key 'normal  dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(evil-define-key 'normal  dired-mode-map (kbd "T") 'dired-tree-down)
(evil-define-key 'normal  dired-mode-map (kbd "~") '(lambda ()(interactive)(find-alternate-file "~/")))
(evil-define-key 'normal  dired-mode-map (kbd "r") 'revert-buffer)
(evil-define-key 'normal  dired-mode-map (kbd "C-r") 'dired-do-redisplay)
(evil-define-key 'normal  dired-mode-map (kbd "gg") 'vinegar/back-to-top)
(evil-define-key 'normal  dired-mode-map (kbd "G") 'vinegar/jump-to-bottom)
(evil-define-key 'normal  dired-mode-map (kbd "f") 'counsel-find-file)
(evil-define-key 'normal  dired-mode-map (kbd "C-f") 'find-name-dired)
(evil-define-key 'normal  dired-mode-map (kbd "H") 'diredp-dired-recent-dirs)
(evil-define-key 'normal  dired-mode-map (kbd "J") 'dired-goto-file)
;; "j"         'vinegar/move-down
;; "k"         'vinegar/move-up
;; "-"         'vinegar/up-directory
;; "0"         'dired-back-to-start-of-files
;; "="         'vinegar/dired-diff
;; (kbd "C-j") 'dired-next-subdir
;; (kbd "C-k") 'dired-prev-subdir
;; "I"         'vinegar/dotfiles-toggle
;; (kbd "~")   '(lambda ()(interactive) (find-alternate-file "~/"))
;; (kbd "RET") (if vinegar-reuse-dired-buffer
;;         'dired-find-alternate-file
;;       'dired-find-file)
;; "f"         (if (configuration-layer/layer-used-p 'ivy)
;;         'counsel-find-file
;;       'helm-find-files)
;; "J"         'dired-goto-file
;; (kbd "C-f") 'find-name-dired
;; "H"         'diredp-dired-recent-dirs
;; "T"         'dired-tree-down
;; "K"         'dired-do-kill-lines
;; "r"         'revert-buffer
;; (kbd "C-r") 'dired-do-redisplay
;; "gg"        'vinegar/back-to-top
;; "G"         'vinegar/jump-to-bottom

(provide 'init-dired)
