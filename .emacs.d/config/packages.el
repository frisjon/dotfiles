;;https://emacs.stackexchange.com/questions/29096/how-to-sort-directories-first-in-dired
(use-package ls-lisp
  :config
  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-use-insert-directory-program nil))
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(use-package rotate
  :config
  (global-set-key (kbd "C-c w r") 'rotate-window))

(use-package ido
  :config
  (ido-mode t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-show-early-on-C-h t))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-<") 'mc/mark-next-like-this)
  (global-set-key (kbd "C->") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package windmove
  :config (windmove-mode t))

;;http://xahlee.info/emacs/emacs/emacs_dired_tips.html
(use-package dired
  :config
  (if (< emacs-major-version 28)
      (progn
        (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
        (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))) ; was dired-up-directory
        )
    (progn
      (setq dired-kill-when-opening-new-dired-buffer t))))

;;eglot-format
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package crux
  :ensure t
  :config
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  ;;(global-set-key (kbd "C-c o") #'crux-open-with)
  (global-set-key [(shift return)] #'crux-smart-open-line)
  ;;(global-set-key (kbd "s-r") #'crux-recentf-find-file)
  ;;(global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line))

(use-package elfeed
  :ensure t
  
  :config
  ;; Mark all YouTube entries
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url "youtube\\.com"
                                :add '(yt)))
  ;; Entries older than 2 weeks are marked as read
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "2 months ago"
                                :remove 'unread))
  
  ;;(add-hook 'elfeed-new-entry-hook
  ;;        (elfeed-make-tagger :feed-url "example\\.com"
  ;;                            :entry-title '(not "something interesting")
  ;;                            :add 'junk
  ;;                            :remove 'unread)) 
  (defun elfeed-search-browse-all-url ()
    (interactive)
    (if (< (count-lines (point-min) (point-max)) 20)
        (progn
          (mark-whole-buffer)
          (elfeed-search-browse-url))
      (message "More than 20 links to open. Not proceeding")))
  (define-key elfeed-search-mode-map "B" 'elfeed-search-browse-all-url))

(use-package project
  :ensure t)

(use-package company
  :ensure t

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))
