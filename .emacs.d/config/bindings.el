(global-set-key (kbd "C-x C-b") 'ibuffer)

;;(global-set-key (kbd "C-c w") 'whitespace-cleanup)
;(global-set-key (kbd "C-c r") 'remember)
(global-set-key (kbd "C-c c") 'crux-cleanup-buffer-or-region)
(global-set-key (kbd "C-c b") 'bookmark-bmenu-list)
(global-set-key (kbd "C-c l") 'goto-line)

(global-set-key (kbd "C-c w v") 'split-window-right)
(global-set-key (kbd "C-c w h") 'split-window-below)
(global-set-key (kbd "C-c w k") 'delete-window)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "C-<backspace>") 'fris/backward-kill-char-or-word)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-S-<tab>") (lambda () (interactive) (other-window -1)))

(global-set-key (kbd "C-c n") 'fris/xah-new-empty-buffer)

;;(global-set-key (kbd "C-x C-S-f") 'fris/find-file-wsl)

(global-set-key (kbd "C-S-k") 'custom/kill-this-buffer)
