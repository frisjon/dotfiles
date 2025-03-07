;; -*- lexical-binding: t; -*-
(require 'use-package)

;; tramp
(use-package tramp
  :defer 5
  :config
  (setq tramp-default-method "plinkx")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path "/home/mk/.cargo/bin"))

;; isearch
(use-package isearch
  :config
  (setq lazy-highlight-cleanup nil
        isearch-wrap-pause 'no))

;; fido
(use-package icomplete
  :defer 1
  :config
  (fido-vertical-mode))

;; ls-lisp
(use-package ls-lisp
  :config
  (setq-default ls-lisp-dirs-first t
                ls-lisp-use-insert-directory-program nil)
  :hook
  (dired-mode . dired-hide-details-mode))

(use-package rotate
  :config
  (setq rotate-functions '(rotate:main-vertical)))

(use-package windmove
  :config
  (windmove-mode t))

;;http://xahlee.info/emacs/emacs/emacs_dired_tips.html
(use-package dired
  :config
  (setq dired-kill-when-opening-new-dired-buffer t
        dired-clean-confirm-killing-deleted-buffers nil
        dired-confirm-shell-command nil
        dired-no-confirm t
        dired-recursive-deletes 'always
        dired-deletion-confirmer '(lambda (x) t)))

(use-package eglot
  :config
  (with-eval-after-load "eglot"
    (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
    (add-to-list 'eglot-server-programs '((rust-mode) . ("rust-analyzer​")))
    (add-to-list 'eglot-server-programs
                 '((rust-ts-mode rust-mode) .
                   ("rust-analyzer" :initializationOptions (:check (:command "clippy"))))))
  :hook
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  (rust-mode . eglot-ensure))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package electric
  :config
  (electric-indent-mode t))

(use-package delsel
  :config
  (delete-selection-mode t))

(use-package elec-pair
  :config
  (setq electric-pair-pairs
        '((?\" . ?\")
          (?\[ . ?\])
          (?\{ . ?\})))
  (electric-pair-mode t))

(use-package paren
  :config
  (show-paren-mode t))

(use-package simple
  :config
  (setq-default indent-tabs-mode nil)
  (column-number-mode))

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
           :html-head "<link rel=\"stylesheet\" href=\"../../res/style.css\" type=\"text/css\"/>"))
        org-footnote-section nil
        org-html-postamble-format
        '(("en" "<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Date: %d</p>
<p class=\"creator\">%c</p>")))
  (org-element-cache-reset))

(use-package tab-bar
  :config
  (setq-default tab-bar-close-button-show nil
                tab-bar-tab-hints t))

(use-package window
  :config
  (setq-default pop-up-windows nil)
  (advice-add 'other-window :before
              (defun other-window-split-if-single (&rest _)
                "Split the frame if there is a single window."
                (when (one-window-p) (split-window-sensibly)))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode t)
  ;;(dolist (mode '(org-mode-hook
                  ;;dired-mode-hook
                  ;;term-mode-hook
                  ;;buffer-list-update-hook
                  ;;help-mode-hook
                  ;;eshell-mode-hook))
    ;;(add-hook mode #'(lambda () (display-line-numbers-mode -1))))
  )

;; for long line files
(global-so-long-mode 1)

;; hl-line
(global-hl-line-mode t)

;; saveplace
;;(save-place-mode nil)

(use-package savehist
  :config
  (setq savehist-file "~/.emacs.d/savehist"
        history-length t
        history-delete-duplicates t
        savehist-save-minibuffer-history 1
        savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring))
  (savehist-mode t))

(use-package autorevert
  :config
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode t))

;; Dont warn for following symlinked files
(use-package vc-hooks
  :config
  (setq vc-follow-symlinks t))

(use-package fringe
  :config
  (fringe-mode '(10 . 0)))

(use-package time
  :config
  (display-time-mode t)
  (setq-default display-time-format "%a %d %b %H:%M"
                display-time-default-load-average nil
                display-time-load-average ""))

(use-package ido
  :config
  (ido-mode t)
  (ido-everywhere t))

(use-package hideshow
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
  ;; https://www.emacswiki.org/emacs/IbufferMode#h5o-1
  ;; Use human readable Size column instead of original one
  :config
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

(load-file "~/.emacs.d/lisp/hideshowvis.el")
(require 'hideshowvis)
(hideshowvis-minor-mode)

(load-file "~/.emacs.d/lisp/selection-highlight-mode.el")
(selection-highlight-mode)

(load-file "~/.emacs.d/lisp/ibuffer-vc.el")
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              vc-relative-file))
      ibuffer-vc-skip-if-remote nil)

;;(load-file "~/.emacs.d/lisp/zen-mode.el")
;;(require 'column-marker)

;; themes
(push "~/.emacs.d/themes" load-path)
;;(load-file "~/.emacs.d/themes/one-themes.el")
;;(load-theme 'one-light)

;;(load-file "~/.emacs.d/themes/tsoding-theme.el")
;;(load-theme 'tsoding)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package edwina
;;  :ensure t
;;  :init
;;  (setq display-buffer-base-action '(display-buffer-below-selected))
;;  :config
;;  (edwina-setup-dwm-keys)
;;  (edwina-mode 1)
;;  (defun edwina-mode-line-indicator ()
;;    "redefining this func to return empty string" ""))

(use-package which-key
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

;; vertico
;; broke when upgrading from 29.x to 30.1 (probably just needed reinstalling)
;; replaced with fido-vertical-mode (2025-03-07)
;;(use-package vertico
;;  :ensure t
;;  :defer 1
;;  :config
;;  (vertico-mode))

;; rust mode
;;(use-package rust-mode
;;  :ensure t
;;  :defer 2)
