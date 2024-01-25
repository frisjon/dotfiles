;;https://emacs.stackexchange.com/questions/29096/how-to-sort-directories-first-in-dired
(use-package startup
  :hook
  (emacs-startup . fris/emacs-startup-time))

(use-package ls-lisp
  :config
  (setq-default ls-lisp-dirs-first t
                ls-lisp-use-insert-directory-program nil)
  :hook
  (dired-mode . dired-hide-details-mode))

(use-package rotate
  :config
  (setq rotate-functions '(rotate:main-vertical))
  :bind
  ("C-c r" . rotate-window))

(use-package ido
  :config
  (ido-mode t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq-default which-key-show-early-on-C-h t))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C-<" . mc/mark-next-like-this)
   ("C->" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

(use-package windmove
  :config
  (windmove-mode t))

;;http://xahlee.info/emacs/emacs/emacs_dired_tips.html
(use-package dired
  :config
  (if (< emacs-major-version 28)
      (progn
        ;; was dired-advertised-find-file
        (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
        ;; was dired-up-directory
        (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))
      (setq dired-kill-when-opening-new-dired-buffer t)))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  :hook
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure))
  ;;(add-hook 'c-mode-hook 'eglot-ensure)
  ;;(add-hook 'c++-mode-hook 'eglot-ensure))

(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode))

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

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package elfeed
  :config
  (defun elfeed-search-browse-all-url ()
    (interactive)
    (if (< (count-lines (point-min) (point-max)) 20)
        (progn
          (mark-whole-buffer)
          (elfeed-search-browse-url))
      (message "More than 20 links to open. Not proceeding")))
  (define-key elfeed-search-mode-map "B" 'elfeed-search-browse-all-url)
  ;; Mark all YouTube entries
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url "youtube\\.com"
                                :add '(video youtube)))
  ;; Entries older than 2 weeks are marked as read
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "1 month ago"
                                :remove 'unread))
  )


(use-package project)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package electric
  :config
  (electric-indent-mode t))

(use-package delsel
  :config
  (delete-selection-mode t))

(use-package elec-pair
  :config
  (electric-pair-mode t)
  (setq electric-pair-pairs
        '(
          (?\" . ?\")
          (?\' . ?\')
          (?< . ?>)
          (?\{ . ?\}))))

(use-package paren
  :ensure t
  :config
  (show-paren-mode t))

(use-package simple
  :config
  (column-number-mode)
  (setq-default indent-tabs-mode nil)
  :bind
  ("C-c l" . goto-line))

(use-package cc-vars
  :config
  (setq-default c-basic-offset 4))

(use-package org
  :config
  (setq-default org-support-shift-select t)
  (setq org-publish-project-alist
    '(("orgblog_test"
        :base-directory "/plinkx:local_wsl:~/orgblog_test/"
        :publishing-function org-html-publish-to-html
        :publishing-directory "/plinkx:local_wsl:~/orgblog_test/public/"
        :section-numbers t
        :recursive t
        :with-toc t
        :html-preamble t
        :exclude "private.org"
        :html-head "<link rel=\"stylesheet\"
                    href=\"../../res/style.css\"
                    type=\"text/css\"/>"))))

(use-package tab-bar
  :config
  (setq-default tab-bar-close-button-show nil
                tab-bar-tab-hints t))

(use-package window
  :config
  (setq-default pop-up-windows nil)
  :bind
  (("C-<tab>" . other-window)
   ("C-S-<tab>" . (lambda () (interactive) other-window -1))))

(use-package isearch
  :bind
  (("C-s" . isearch-forward-regexp)
   ("C-r" . replace-regexp)))

(use-package menu-bar
  :config
  (menu-bar-mode -1))

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package scroll-bar
  :config
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1))

(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode t)
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  help-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

(use-package hl-line
  :config
  (global-hl-line-mode 1))

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package savehist
  :config
  (savehist-mode 1))

;; update buffers from disk
(use-package autorevert
  :config
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t))

;; Dont warn for following symlinked files
(use-package vc-hooks
  :config
  (setq vc-follow-symlinks t))

(use-package time
  :config
  (display-time-mode t)
  (setq-default display-time-format " %a %d %b %H:%M"
                display-time-default-load-average nil
                display-time-load-average ""))

;;(display-time-next-load-average)

(use-package misc
  :bind
  ("C-S-d" . duplicate-line))

(use-package bookmark
  :bind
  ("C-c b" . bookmark-bmenu-list))

(use-package crux
  :bind
  ("C-c c" . crux-cleanup-buffer-or-region))

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer))

(use-package whitespace
  :bind
  ("C-c w" . whitespace-cleanup))

(use-package edwina
  :ensure t
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  (edwina-setup-dwm-keys)
  (edwina-mode 1)
  (defun edwina-mode-line-indicator ()
    "redefining this func" "")
  )

(use-package erc
  :config
  (setq erc-nick "fris"
        erc-system-name "frispc"))
