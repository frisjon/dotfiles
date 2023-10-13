;; -*- lexical-binding: t; -*-

(make-directory "~/.emacs.d/backups/" t)
(make-directory "~/.emacs.d/autosave/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
;;(setq auto-save-default nil)

(setq default-directory "~/")

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

(push "~/.emacs.d/lisp" load-path)

(set-default-coding-systems 'utf-8)

;; move customization to file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(setq inhibit-startup-screen t)
(setq visible-bell nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode 1)

;; line numbers
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-hl-line-mode 1)

;; theme
;;(use-package modus-themes :ensure t)
;;(load-theme 'modus-operandi)
;; requires font source code pro to be install
(add-to-list 'default-frame-alist '(font . "Consolas-14"))
(when (require 'sanity nil 'noerror))
;;(when (require 'elegance nil 'noerror))

;; command history
(setq history-length 25)
(savehist-mode 1)

;; supress dialog box
(setq use-dialog-box nil)

;; update buffers from disk
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;;https://emacs.stackexchange.com/questions/29096/how-to-sort-directories-first-in-dired
(use-package ls-lisp
  :config
  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-use-insert-directory-program nil))
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(global-set-key (kbd "C-c w v") 'split-window-right)
(global-set-key (kbd "C-c w h") 'split-window-below)
(global-set-key (kbd "C-c w k") 'delete-window)

(use-package rotate
  :config
  (global-set-key (kbd "C-c w r") 'rotate-window))

;;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Remote-shell-setup.html
;;(add-to-list 'tramp-connection-properties (list (regexp-quote "/plinkx:local_wsl:") "remote-shell" "/bin/bash"))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

;;(require 'use-package)
;;(setq use-package-always-ensure t)

;;(use-package command-log-mode)

;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;;(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Dont warn for following symlinked files
(setq vc-follow-symlinks t)

(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)

(use-package ido
  :config
  (ido-mode t))

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(electric-indent-mode t)
;;(whitespace-mode t)
(setq show-trailing-whitespace t)
(delete-selection-mode t)
(electric-pair-mode t)
(show-paren-mode t)
(column-number-mode)
(setq-default tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq c-basic-offset 4)

(setq electric-pair-pairs
      '(
        (?\" . ?\")
        (?\' . ?\')
        (?< . ?>)
        (?\{ . ?\})))

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

;;https://github.com/Wilfred/helpful
;;https://github.com/pashinin/workgroups2
;;https://github.com/mrkkrp/ace-popup-menu
;;https://github.com/abo-abo/ace-window
;;https://github.com/emacsorphanage/god-mode
;;https://github.com/benma/visual-regexp-steroids.el/

(use-package windmove
  :config (windmove-mode t))

(defun fris/backward-kill-char-or-word ()
  "https://emacs.stackexchange.com/questions/30401/backward-kill-word-kills-too-much-how-to-make-it-more-intelligent"
  (interactive)
  (cond
   ((looking-back (rx (char word)) 1)
    (backward-kill-word 1))
   ((looking-back (rx (char blank)) 1)
    (delete-horizontal-space t))
   (t
    (backward-delete-char 1))))

(global-set-key (kbd "C-<backspace>") 'fris/backward-kill-char-or-word)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-S-<tab>") (lambda () (interactive) (other-window -1)))

;;http://xahlee.info/emacs/emacs/emacs_dired_tips.html
(use-package dired
  :config
  (if (< emacs-major-version 28)
      (progn
        (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
        (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))) ; was dired-up-directory
        )
    (progn
      (setq dired-kill-when-opening-new-dired-buffer t)))
  
  )

(defun fris/xah-new-empty-buffer ()
  "Create a new empty buffer.
Returns the buffer object.
New buffer is named untitled, untitled<2>, etc.

Warning: new buffer is not prompted for save when killed, see `kill-buffer'.
Or manually `save-buffer'

URL `http://xahlee.info/emacs/emacs/emacs_new_empty_buffer.html'
Version: 2017-11-01 2022-04-05"
  (interactive)
  (let ((xbuf (generate-new-buffer "untitled")))
    (switch-to-buffer xbuf)
    (funcall initial-major-mode)
    xbuf))
(global-set-key (kbd "C-c n") 'fris/xah-new-empty-buffer)

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure))

;;https://emacs.stackexchange.com/questions/28906/how-to-switch-off-the-sounds
(setq ring-bell-function 'ignore)

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

(defun fris/find-file-wsl ()
  (interactive)
  (find-file "/plinkx:local_wsl:~/"))

;;(global-set-key (kbd "C-x C-S-f") 'fris/find-file-wsl)

(use-package project)

(put 'dired-find-alternate-file 'disabled nil)

(defalias 'yes-or-no-p 'y-or-n-p) ;;https://www.emacswiki.org/emacs/YesOrNoP
(defalias 'l 'eshell/ls)
(defalias 'll (lambda () (eshell/ls "-l" )))
(defalias 'la (lambda () (eshell/ls "-la")))
(defalias 'less (lambda (a) (view-file a)))


;; mode-line

(setq org-support-shift-select t)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-c w") 'whitespace-cleanup)
(global-set-key (kbd "C-c r") 'remember)
(global-set-key (kbd "C-c b") 'bookmark-bmenu-list)
(global-set-key (kbd "C-c l") 'goto-line)
