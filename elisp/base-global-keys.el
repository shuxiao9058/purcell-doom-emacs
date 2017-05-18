;; Add your keys here, as such

;(global-set-key (kbd "[SHORTCUT]") '[FUNCTION])
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-x M-SPC") 'pop-global-mark)
(global-set-key (kbd "C-{") 'origami-toggle-node)
(global-set-key (kbd "C-S-O") 'origami-toggle-all-nodes)


(provide 'base-global-keys)
