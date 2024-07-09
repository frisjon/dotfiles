;; -*- lexical-binding: t; -*-
(use-package tramp
  :ensure nil
  :defer t
  :init
  (setq tramp-default-method "plinkx")
  :config
  (with-eval-after-load "tramp"
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
    (add-to-list 'tramp-remote-path "/home/mk/.cargo/bin")))

(use-package isearch
  :ensure nil
  :init
  (setq lazy-highlight-cleanup nil
        isearch-wrap-pause 'no
        ))

(use-package ls-lisp
  :ensure nil
  :defer 5
  :init
  (setq-default
   ls-lisp-dirs-first t
   ls-lisp-use-insert-directory-program nil)
  :hook
  (dired-mode . dired-hide-details-mode))

;;(require 'rotate)
(use-package rotate
  :ensure nil
  :defer t
  :init
  (setq rotate-functions '(rotate:main-vertical)))

;;(require 'windmove)
(use-package windmove
  :ensure nil
  :defer 1
  :config
  (windmove-mode t))

;;http://xahlee.info/emacs/emacs/emacs_dired_tips.html
;;(require 'dired)
(use-package dired
  :ensure nil
  :defer 2
  :init
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

;;(require 'eglot)
(use-package eglot
  :ensure nil
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
  (rust-mode . eglot-ensure))

;;(require 'uniquify)
(use-package uniquify
  :ensure nil
  :defer 3
  :init
  (setq uniquify-buffer-name-style 'forward))

;;(require 'project)
(use-package project
  :ensure nil
  :defer 4)

;;(require 'electric)
(use-package electric
  :ensure nil
  :defer 2
  :config
  (electric-indent-mode t))

;;(require 'delsel)
(use-package delsel
  :ensure nil
  :defer 2
  :config
  (delete-selection-mode t))

;;(require 'elec-pair)
(use-package elec-pair
  :ensure nil
  :defer 1
  :init
  (setq electric-pair-pairs
        '((?\" . ?\")
          (?\[ . ?\])
          (?\{ . ?\})))
  :config
  (electric-pair-mode t))

;;(require 'paren)
(use-package paren
  :ensure nil
  :defer 1
  :config
  (show-paren-mode t))

;;(require 'simple)
(use-package simple
  :ensure nil
  :defer t
  :init
  (setq-default indent-tabs-mode nil)
  :config
  (column-number-mode))

;;(require 'cc-vars)
(use-package cc-vars
  :ensure nil
  :defer t
  :init
  (setq-default c-basic-offset 4))

;;(require 'org)
(use-package org
  :ensure nil
  :defer t
  :init
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
           :html-head "<link rel=\"stylesheet\" href=\"../../res/style.css\" type=\"text/css\"/>"))
        org-footnote-section nil
        org-html-postamble-format
        '(("en" "<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Date: %d</p>
<p class=\"creator\">%c</p>")))
  :config
  (org-element-cache-reset))

;;(require 'tab-bar)
(use-package tab-bar
  :ensure nil
  :defer t
  :init
  (setq-default tab-bar-close-button-show nil
                tab-bar-tab-hints t))

;;(require 'window)
(use-package window
  :ensure nil
  :defer 1
  :init
  (setq-default pop-up-windows nil)
  :config
  (advice-add 'other-window :before
              (defun other-window-split-if-single (&rest _)
                "Split the frame if there is a single window."
                (when (one-window-p) (split-window-sensibly))))
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
  :ensure nil
  :defer 1
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
  :ensure nil
  :defer 1
  :config
  (global-hl-line-mode t))

;;(require 'saveplace)
(use-package saveplace
  :ensure nil
  :defer 1
  :config
  (save-place-mode t))

;;(require 'savehist)
(use-package savehist
  :ensure nil
  :defer 1
  :init
  (setq savehist-file "~/.emacs.d/savehist"
        history-length t
        history-delete-duplicates t
        savehist-save-minibuffer-history 1
        savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring))
  :config
  (savehist-mode t))

;; update buffers from disk
;;(require 'autorevert)
(use-package autorevert
  :ensure nil
  :init
  (setq global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode t)
  )

;; Dont warn for following symlinked files
;;(require 'vc-hooks)
(use-package vc-hooks
  :ensure nil
  :defer 10
  :init
  (setq vc-follow-symlinks t))

(use-package fringe
  :ensure nil
  :init
  (fringe-mode '(10 . 0)))

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
  :ensure nil
  :defer t
  :init
  (setq erc-nick "kneecaps"
        erc-system-name "potato"
        erc-hide-list '("JOIN" "PART" "QUIT")))

;;(require 'recentf)
;;(recentf-mode t)

(use-package edwina
  :ensure t
  :init
  (setq display-buffer-base-action '(display-buffer-below-selected))
  :config
  (edwina-setup-dwm-keys)
  (edwina-mode 1)
  (defun edwina-mode-line-indicator ()
    "redefining this func to return empty string" ""))

(use-package which-key
  :ensure t
  :init
  (setq-default which-key-show-early-on-C-h t)
  :config
  (which-key-mode))

(use-package multiple-cursors
  :ensure t
  :defer 1
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C-<" . mc/mark-next-like-this)
   ("C->" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

(use-package company
  :ensure t
  :defer 3
  ;;:custom-face
  ;;(company-tooltip ((t (:family "Noto Sans Mono"))))
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
  ;;(global-set-key (kbd "C-c o") #'crux-open-with)
  ;;(global-set-key (kbd "s-r") #'crux-recentf-find-file)
  ;;(global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  (global-set-key [(shift return)] #'crux-smart-open-line)
  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line))

(use-package editorconfig
  :ensure t
  :defer 1
  :config
  (editorconfig-mode 1))

(use-package vertico
  :ensure t
  :defer 1
  :config
  (vertico-mode))

(use-package rust-mode
  :ensure t
  :defer 2)

(use-package ido
  :ensure nil
  :config
  (ido-mode t))

(use-package hideshow
  :ensure nil
  :defer 1
  :config
  (setq
    hs-hide-comments nil
    hs-isearch-open 'x)
  (defvar hs-special-modes-alist
    (mapcar 'purecopy
      '((c-mode "{" "}" "/[*/]" nil nil)
         (rust-mode "{" "}" "/[*/]" nil nil)
         (c++-mode "{" "}" "/[*/]" nil nil)
         ;;(emacs-lisp-mode "(" ")" nil nil nil nil)
         ;;(bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
         (java-mode "{" "}" "/[*/]" nil nil)
         (js-mode "{" "}" "/[*/]" nil))))
  ;;(hs-minor-mode)
  (dolist
    (prog-modes '(c-mode-common-hook
                   emacs-lisp-mode-hook
                   rust-mode-hook
                   c++-mode-hook
                   js-mode-hook
                   java-mode-hook))
    (add-hook prog-modes 'hs-minor-mode))
  )

(use-package ibuffer
  :ensure nil
  :config
  ;; https://www.emacswiki.org/emacs/IbufferMode#h5o-1
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size"
           :inline t
           :summarizer
           (lambda (column-strings)
             (let ((total 0))
               (dolist (string column-strings)
                 (setq total
                       ;; like, ewww ...
                       (+ (float (ajv/human-readable-file-sizes-to-bytes string))
                          total)))
               (ajv/bytes-to-human-readable-file-sizes total)))    ;; :summarizer nil
           )
    (ajv/bytes-to-human-readable-file-sizes (buffer-size)))

  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 20 20 :left :elide) " "
                ;;(size-h 11 -1 :right) " "
                (mode 16 16 :left :elide) " "
                filename-and-process)
          (mark " "
                (name 16 -1) " " filename))
        ibuffer-default-sorting-mode 'major-mode))

;;(load-file "~/.emacs.d/lisp/hideshowvis.el")
(use-package hideshowvis
  :ensure nil
  :defer 3
  ;;:config (hideshowvis-minor-mode)
  )

;;(load-file "~/.emacs.d/lisp/selection-highlight-mode.el")
(use-package selection-highlight-mode
  :ensure nil
  :defer 2
  :config
  (selection-highlight-mode))

;;(load-file "~/.emacs.d/lisp/ibuffer-vc.el")
;;(use-package ibuffer-vc
;;  :ensure nil
;;  :config
;;  (setq ibuffer-formats
;;        '((mark modified read-only vc-status-mini " "
;;                (name 18 18 :left :elide)
;;                " "
;;                (size 9 -1 :right)
;;                " "
;;                (mode 16 16 :left :elide)
;;                " "
;;                (vc-status 16 16 :left)
;;                " "
;;                vc-relative-file))
;;        ibuffer-vc-skip-if-remote nil))

;;(require 'column-marker)

;; themes
(push "~/.emacs.d/themes" load-path)
;;(load-file "~/.emacs.d/themes/one-themes.el")
;;(load-theme 'one-light)

;;(load-file "~/.emacs.d/themes/tsoding-theme.el")
;;(load-theme 'tsoding)
