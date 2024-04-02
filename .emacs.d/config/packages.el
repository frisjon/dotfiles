(use-package tramp
  :init
  (with-eval-after-load "tramp"
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
    (add-to-list 'tramp-remote-path "/home/mk/.cargo/bin")
    )
  :config
  (setq tramp-default-method "plinkx")
  )

(use-package edwina
  :ensure t
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  (edwina-setup-dwm-keys)
  (edwina-mode 1)
  (defun edwina-mode-line-indicator ()
    "redefining this func to return empty string" ""))

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

(use-package company
  :ensure t
  :defer 3
  :custom-face
  (company-tooltip ((t (:family "Noto Sans Mono"))))
  :hook
  (after-init . global-company-mode))

(use-package move-text
  :ensure t
  :defer 2
  :config
  (move-text-default-bindings))

(use-package crux
  :ensure t
  :defer 1
  :config
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  ;;(global-set-key (kbd "C-c o") #'crux-open-with)
  (global-set-key [(shift return)] #'crux-smart-open-line)
  ;;(global-set-key (kbd "s-r") #'crux-recentf-find-file)
  ;;(global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line))

(use-package elfeed
  :ensure t
  :defer t
  :config
  (defun fris/elfeed-search-browse-all-url ()
    (interactive)
    (if (< (count-lines (point-min) (point-max)) 20)
        (progn
          (mark-whole-buffer)
          (elfeed-search-browse-url))
      (message "More than 20 links to open. Not proceeding")))
  (define-key elfeed-search-mode-map "B" 'fris/elfeed-search-browse-all-url)
  ;; Mark all YouTube entries
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url "youtube\\.com"
                                :add '(video youtube)))
  ;; Entries older than 2 weeks are marked as read
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "1 month ago"
                                :remove 'unread)))

(use-package editorconfig
  :ensure t
  :defer 1
  :config
  (editorconfig-mode 1))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

;;(require 'ls-lisp)
(use-package ls-lisp
  :defer 5
  :config
  (setq-default
   ls-lisp-dirs-first t
   ls-lisp-use-insert-directory-program nil)
  :hook
  (dired-mode . dired-hide-details-mode))

;;(require 'rotate)
(use-package rotate
  :defer t
  :config
  (setq rotate-functions '(rotate:main-vertical)))

;;(require 'ido)
(use-package ido
  :defer 1
  :config
  (ido-mode t)
  )

;;(require 'windmove)
(use-package windmove
  :defer 1
  :config
  (windmove-mode t)
  )

;;http://xahlee.info/emacs/emacs/emacs_dired_tips.html
;;(require 'dired)
(use-package dired
  :config
  ;;(if (< emacs-major-version 28)
  ;;    (progn
  ;;      ;; was dired-advertised-find-file
  ;;      (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  ;;      ;; was dired-up-directory
  ;;      (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))))
  (setq dired-kill-when-opening-new-dired-buffer t
        dired-clean-confirm-killing-deleted-buffers nil
        dired-confirm-shell-command nil
        dired-no-confirm t
        dired-recursive-deletes 'always
        dired-deletion-confirmer '(lambda (x) t))
  )

(use-package rust-mode
  :ensure t
  )

;;(require 'eglot)
(use-package eglot
  :defer 5
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-to-list 'eglot-server-programs '((rust-mode) . ("rust-analyzer​")))
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
  :hook
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  (rust-mode . eglot-ensure)
  )


;;(require 'uniquify)
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  )

;;(require 'project)

;;(require 'electric)
(use-package electric
  :defer 1
  :config
  (electric-indent-mode t)
  )

;;(require 'delsel)
(use-package delsel
  :defer 1
  :config
  (delete-selection-mode t)
  )

;;(require 'elec-pair)
(use-package elec-pair
  :defer 1
  :config
  (electric-pair-mode t)
  (setq electric-pair-pairs
        '((?\" . ?\")
          (?\[ . ?\])
          (?\{ . ?\})))
  )

;;(require 'paren)
(use-package paren
  :config
  (show-paren-mode t)
  )

;;(require 'simple)
(use-package simple
  :defer t
  :config
  (column-number-mode)
  (setq-default indent-tabs-mode nil)
  )

;;(require 'cc-vars)
(use-package cc-vars
  :defer t
  :config
  (setq-default c-basic-offset 4)
  )

;;(require 'org)
(use-package org
  :defer t
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
                    type=\"text/css\"/>")))
  (setq org-html-postamble-format
        '(("en" "<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Date: %d</p>
<p class=\"creator\">%c</p>")))
  (progn
    (setq org-footnote-section nil)
    (org-element-cache-reset)
    )
  )

;;(require 'tab-bar)
(use-package tab-bar
  :defer t
  :config
  (setq-default tab-bar-close-button-show nil
                tab-bar-tab-hints t)
  )

;;(require 'window)
(use-package window
  :defer 1
  :config
  (setq-default pop-up-windows nil)
  )

;;(require 'menu-bar)
(menu-bar-mode -1)

;;(require 'tool-bar)
(tool-bar-mode -1)

;;(require 'scroll-bar)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;;(require 'display-line-numbers)
(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode t)

  ;;(dolist (mode '(org-mode-hook
  ;;                 dired-mode-hook
  ;;                 term-mode-hook
  ;;                 buffer-list-update-hook
  ;;                 help-mode-hook
  ;;                 eshell-mode-hook))
  ;;  (add-hook mode #'(lambda () (display-line-numbers-mode -1))))
  )

;;(require 'hl-line)
(use-package hl-line
  :defer 1
  :config
  (global-hl-line-mode t)
  )

;;(require 'saveplace)
(use-package saveplace
  :defer 1
  :config
  (save-place-mode t)
  )

;;(require 'savehist)
(use-package savehist
  :defer 1
  :config
  (setq savehist-file "~/.emacs.d/savehist")
  (savehist-mode t)
  (setq history-length t)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history 1)
  (setq savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring))
  )

;; update buffers from disk
;;(require 'autorevert)
(use-package autorevert
  :config
  (global-auto-revert-mode t)
  (setq global-auto-revert-non-file-buffers t)
  )

;; Dont warn for following symlinked files
;;(require 'vc-hooks)
(use-package vc-hooks
  :defer 10
  :config
  (setq vc-follow-symlinks t)
  )

;;(require 'time)
;;(use-package time
;;  :defer 1
;;  :config
;;  (display-time-mode t)
;;  (setq-default display-time-format "%a %d %b %H:%M"
;;              display-time-default-load-average nil
;;              display-time-load-average "")
;;  )

;;(require 'erc)
(use-package erc
  :defer t
  :config
  (setq erc-nick "kneecaps"
        erc-system-name "potato"
        erc-hide-list '("JOIN" "PART" "QUIT"))
  )

;;(require 'recentf)
;;(recentf-mode t)

;; themes
(push "~/.emacs.d/themes" load-path)
(load-file "~/.emacs.d/themes/ef-themes.el")
;;(use-package ef-themes
;;  :defer 1
;;  :config
;;  (ef-themes-select 'ef-kassio)
;;  ;;(when 'ef-kassio (ef-themes-select 'ef-kassio))
;;  )

(load-file "~/.emacs.d/lisp/selection-highlight-mode.el")
(use-package selection-highlight-mode
  :config (selection-highlight-mode))

(load-file "~/.emacs.d/lisp/exec-path-from-shell.el")
(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

(use-package gruvbox-theme
  :ensure t)
