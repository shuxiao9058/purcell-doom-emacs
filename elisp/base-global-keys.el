;; Add your keys here, as such

;(global-set-key (kbd "[SHORTCUT]") '[FUNCTION])
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-x M-SPC") 'pop-global-mark)
(global-set-key (kbd "C-{") 'origami-toggle-node)
(global-set-key (kbd "C-S-O") 'origami-toggle-all-nodes)

(global-set-key (kbd "C-M-=") 'cfs-increase-fontsize)
(global-set-key (kbd "C-M--") 'cfs-decrease-fontsize)

;; (use-package default-text-scale
;;   :bind
;;   ("C-M-=" . default-text-scale-increase)
;;   ("C-M--" . default-text-scale-decrease))

(provide 'base-global-keys)
