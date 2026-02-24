;; -*- lexical-binding: t; -*-
;;(load-file "~/.emacs.d/lisp/emacs-load-time.el")
;;(require 'emacs-load-time)

(package-initialize)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(unless package-archive-contents
 (package-refresh-contents))

(setq gc-cons-threshold (* 2 1000 1000 1000))
(make-directory "~/.emacs.d/config/" t)
(push "~/.emacs.d/config" load-path)

;; Initialize package sources
(use-package package
  :config
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("elpa" . "https://elpa.gnu.org/packages/"))))

;; Settings --------------------------------------------------------------------
(setq default-directory "~/")

(make-directory "~/.emacs.d/backups/" t)
(make-directory "~/.emacs.d/autosave/" t)
(make-directory "~/.emacs.d/lisp/" t)

(push "~/.emacs.d/lisp" load-path)

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      initial-scratch-message nil)

(use-package display-fill-column-indicator
  :config
  (display-fill-column-indicator-mode))

(use-package files
  :config
  (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t))
        backup-directory-alist '(("." . "~/.emacs.d/backups"))
        ;;auto-save-default nil
        backup-by-copying t))

(setq coding-system-for-read 'utf-8-unix
      coding-system-for-write 'utf-8-unix

      visible-bell nil
      history-length 25 ;; command history
      use-dialog-box nil ;; supress dialog box
      ;;https://emacs.stackexchange.com/questions/28906/how-to-switch-off-the-sounds
      ring-bell-function 'ignore
      delete-by-moving-to-trash t
      read-buffer-completion-ignore-case t
      fill-column 80
      explicit-shell-file-name "/bin/bash"

      system-time-locale "C"
      eol-mnemonic-unix "-")

(setq-default buffer-file-coding-system 'utf-8-unix ;; coding system)
              tab-width 4
              help-window-select t ;; focus help window
              tab-always-indent 'complete ;; coding system
              show-trailing-whitespace t)

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8)

;; move customization to file?
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; font
;;(add-to-list 'default-frame-alist '(font . "Noto Sans Mono-12"))
;; https://www.youtube.com/watch?v=qR8JRYr4BKE
(dolist (face '(default
                fixed-pitch
                variable-pitch))
  (set-face-attribute face nil
            :family "Noto Sans Mono"
            :height 130))

;; http://xahlee.info/emacs/emacs/emacs_set_font_symbol.html
(set-fontset-font
 t
 'symbol
 (cond ((eq system-type 'windows-nt)
        (cond ((member "Segoe UI Symbol" (font-family-list)) "Segoe UI Symbol")
              ((member "all-the-icons" (font-family-list)) "all-the-icons")))
       ((eq system-type 'gnu/linux)
        (cond ((member "Symbola" (font-family-list)) "Symbola")))))

(set-fontset-font t
'emoji
(cond ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
      ((member "all-the-icons" (font-family-list)) "all-the-icons")
      ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
      ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
      ((member "Symbola" (font-family-list)) "Symbola")))

;; whitespace
(dolist (mode '(dired-mode-hook
                term-mode-hook
                help-mode-hook
                messages-buffer-mode-hook
                eshell-mode-hook))
(add-hook mode (lambda () (setq show-trailing-whitespace -1))))

;;https://www.emacswiki.org/emacs/YesOrNoP
(defalias 'yes-or-no-p 'y-or-n-p)
(fset 'yes-or-no-p 'y-or-n-p)

;; eshell aliases
(defalias 'l 'eshell/ls)
(defalias 'll (lambda () (eshell/ls "-l" )))
(defalias 'la (lambda () (eshell/ls "-la")))
(defalias 'less (lambda (a) (view-file a)))

;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;;(setq scroll-step 1) ;; keyboard scroll one line at a time

(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))
;;(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
;;(add-to-list 'exec-path (expand-file-name "/plinkx:local_wsl:~/.cargo/bin/"))
;;(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))

;;https://emacs.stackexchange.com/questions/29096/how-to-sort-directories-first-in-dired
(add-hook 'emacs-startup-hook 'fris/emacs-startup-time)
(add-hook 'kill-buffer-query-functions (lambda () (not-modified) t))

;; theme stuff
;; remove box attr from modeline
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(defun fris/remove-box-attr-from-modeline ()
  "remove box attribute from modeline"
  (interactive)
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil))

(add-hook 'after-load-theme-hook 'fris/remove-box-attr-from-modeline)

(use-package pixel-scroll
  :config
  (pixel-scroll-precision-mode t))

(add-hook 'prog-mode #'hs-minor-mode) ;; ñ
;; ==============================================================================

;; Functions
(defun fris/emacs-startup-time ()
  "Profile emacs startup"
  (message "*** Emacs loaded in %s seconds with %d garbage collections."
    (emacs-init-time "%.2f") gcs-done))

(defun fris/backward-kill-char-or-word ()
  "To replace default backward-kill-word
Taken from
https://emacs.stackexchange.com/questions/30401/backward-kill-word-kills-too-much-how-to-make-it-more-intelligent"
  (interactive)
  (cond ((looking-back (rx (char word)) 1)
          (backward-kill-word 1))
    ((looking-back (rx (char blank)) 1)
      (delete-horizontal-space t))
    (t (backward-delete-char 1))))

(defun fris/kill-this-buffer ()
  (interactive) (kill-buffer (current-buffer)))

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

(defun fris/edwina-focus-master ()
  "select window with biggest area (presumably master)"
  (interactive)
  (if (window-left-child(frame-root-window))
    (select-window(window-left-child(frame-root-window)))
    (select-window (frame-root-window))))

(defun fris/edwina-new-empty-buffer-in-window ()
  "open new empty buffer in new windows to the left. dwm style"
  (interactive)
  (fris/edwina-focus-master)
  (edwina-clone-window)
  (fris/xah-new-empty-buffer))

(defun fris/find-file-wsl ()
  "find-file in remote location."
  (interactive)
  (find-file "/plinkx:local_wsl:~/"))

(defun fris/edwina-kill-buffer-and-window ()
  (interactive)
  (kill-buffer-and-window)
  (edwina-arrange)
  (fris/edwina-focus-master))

(defun fris/edwina-zoom ()
  (interactive)
  (edwina-zoom)
  (fris/edwina-focus-master))

(defun fris/edwina-delete-window()
  (interactive)
  (edwina-delete-window)
  (fris/edwina-focus-master))

(defun fris/edwina-open-ibuffer-in-new-window()
  (interactive)
  (edwina-clone-window)
  (fris/edwina-focus-master)
  (ibuffer nil))

(defun fris/color-name-string-to-hex-string (color)
  "Takes in string color-name variable and returns its hex code as a string"
  (apply 'format "#%02x%02x%02x"
    (mapcar (lambda (c) (ash c -8))
      (color-values color))))

(defalias 'fris/color 'fris/color-name-string-to-hex-string)

;; write a function to do the spacing
(defun fris-modeline/simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively.
https://emacs.stackexchange.com/questions/5529/how-to-right-align-some-items-in-the-modeline"
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(defun fris/unfill-paragraph ()
  "Taken from
https://www.reddit.com/r/emacs/comments/1act3md/how_to_replace_inparagraph_newlines_etc_with/"
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil)))

(defun fris/insert-character-with-font (char font)
  "Insert CHAR into the current buffer with FONT.
Taken from https://emacs.stackexchange.com/a/79976"
  (let ((start (point)))
    (insert char)
    (add-text-properties start (point)
                         `(face (:family ,font)))))

(defun fris/print-all-fonts ()
  "Print details of all installed fonts.
Taken from https://emacs.stackexchange.com/a/79976"
  (interactive)
  (font-lock-mode -1)
  (set-fontset-font t 'symbol nil)
  (set-fontset-font t 'emoji nil)
  (let ((fonts (font-family-list)))
    (dolist (font fonts)
      (fris/insert-character-with-font "🔁" font) ;; 01F501 - Segoe UI Symbol is best
      (insert " ")
      (fris/insert-character-with-font "🧗" font) ;; 01F9D7 - only in Segoe UI Emoji
      (insert (prin1-to-string font))
      (insert "\n"))))

(defun fris/highlight-word ()
  "Highlight the current word you are on.
https://www.reddit.com/r/emacs/comments/1bgyq3y/wrote_my_first_function/"
  (interactive)
  (backward-word 1)
  (set-mark-command nil)
  (forward-word 1))

(defun fris/xah-next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (fris/xah-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun fris/xah-previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (fris/xah-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun fris/xah-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil. version 2016-06-18"
  (interactive)
  (if (and (string-equal "*" (substring (buffer-name) 0 1)) (not (string-equal "*scratch*" (buffer-name))) (not (string-equal "*Help*" (buffer-name))))
      nil
    t)
  )

(defun ajv/human-readable-file-sizes-to-bytes (string)
  "Convert a human-readable file size into bytes.
https://www.emacswiki.org/emacs/IbufferMode#h5o-1"
  (interactive)
  (cond
   ((string-suffix-p "G" string t)
    (* 1000000000 (string-to-number (substring string 0 (- (length string) 1)))))
   ((string-suffix-p "M" string t)
    (* 1000000 (string-to-number (substring string 0 (- (length string) 1)))))
   ((string-suffix-p "K" string t)
    (* 1000 (string-to-number (substring string 0 (- (length string) 1)))))
   (t (string-to-number (substring string 0 (- (length string) 1))))))

(defun ajv/bytes-to-human-readable-file-sizes (bytes)
  "Convert number of bytes to human-readable file size.
https://www.emacswiki.org/emacs/IbufferMode#h5o-1"
  (interactive)
  (cond
   ((> bytes 1000000000) (format "%10.1fG" (/ bytes 1000000000.0)))
   ((> bytes 100000000) (format "%10.0fM" (/ bytes 1000000.0)))
   ((> bytes 1000000) (format "%10.1fM" (/ bytes 1000000.0)))
   ((> bytes 100000) (format "%10.0fk" (/ bytes 1000.0)))
   ((> bytes 1000) (format "%10.1fk" (/ bytes 1000.0)))
   (t (format "%10d" bytes))))
;; ==============================================================================

;; Bindings ---------------------------------------------------------------------
(global-set-key (kbd "C-c C-SPC") 'fris/highlight-word)
(global-set-key (kbd "M-1") 'delete-other-windows)

(global-unset-key (kbd "M-z"))
;;(global-set-key (kbd "M-z") 'fris/edwina-focus-master)
;;(global-set-key [remap edwina-zoom] #'fris/edwina-zoom)
;;(global-set-key [remap edwina-delete-window] #'fris/edwina-delete-window)
(global-unset-key (kbd "M-q"))
;;(global-set-key (kbd "M-q") 'fris/edwina-delete-window)
;;(global-set-key (kbd "M-Q") 'fris/edwina-kill-buffer-and-window)
;;(global-set-key (kbd "M-a") 'fris/edwina-new-empty-buffer-in-window)
;;(global-set-key (kbd "M-S-a") 'fris/xah-new-empty-buffer)
(global-set-key (kbd "M-A") 'fris/xah-new-empty-buffer)
;;(global-set-key (kbd "M-t") 'fris/edwina-open-shell-in-new-window)
;;(global-unset-key (kbd "M-S-<return>"))
;;(global-set-key (kbd "M-S-<return>") 'fris/edwina-open-ibuffer-in-new-window)
;;(global-set-key (kbd "M-]") 'fris/edwina-open-ibuffer-in-new-window)
;;(eval-after-load 'dired
;;  '(define-key dired-mode-map (kbd "M-]") 'fris/edwina-open-ibuffer-in-new-window))

(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "M-}") 'ibuffer))

(eval-after-load 'org
  '(define-key org-mode-map (kbd "M-}") 'ibuffer))

(global-set-key (kbd "C-c n") 'fris/xah-new-empty-buffer)
(global-set-key (kbd "C-x k") 'fris/kill-this-buffer)

(global-set-key (kbd "C-<backspace>") 'fris/backward-kill-char-or-word)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(global-set-key (kbd "<f1>") 'fris/xah-previous-user-buffer)
(global-set-key (kbd "<f2>") 'fris/xah-next-user-buffer)
;;(global-set-key (kbd "C-x C-S-f") 'fris/find-file-wsl)

(global-set-key (kbd "M-e") 'fill-paragraph)
(global-set-key (kbd "M-E") 'fris/unfill-paragraph)

(eval-after-load 'c-mode '(define-key c-mode-map (kbd "C-c C-l") 'hs-toggle-hiding))
(eval-after-load 'c-mode '(define-key c-mode-map (kbd "C-c C-k") 'hs-hide-all))
(eval-after-load 'c-mode '(define-key c-mode-map (kbd "C-c C-j") 'hs-show-all))
;;(define-key c-mode-map (kbd "C-c C-l") 'hs-toggle-hiding)
;;(define-key c-mode-map (kbd "C-c C-k") 'hs-hide-all)
;;(define-key c-mode-map (kbd "C-c C-j") 'hs-show-all)

;; unbindings
(global-unset-key (kbd "<insertchar>"))
(global-unset-key (kbd "<insert>"))
(global-unset-key (kbd "C-d"))
(global-unset-key (kbd "C-S-d"))
(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-t"))
(global-unset-key (kbd "C-x C-l"))

(if (boundp 'help-mode-map)
    (define-key help-mode-map "q" (lambda () (interactive) (quit-window 1))))

(defadvice quit-window (before quit-window-always-kill)
  "When running `quit-window', always kill the buffer."
  (ad-set-arg 0 t))
(ad-activate 'quit-window)
;; ==============================================================================

;; Packages ---------------------------------------------------------------------
(require 'use-package)

(use-package loaddefs
:bind
("C-S-s" . 'highlight-regexp))

(use-package multiple-cursors
  :ensure t
  :defer 1
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C-<" . mc/mark-next-like-this)
   ("C->" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

(use-package misc
  :bind
  (("C-S-d" . 'duplicate-line)
   ("C-d" . 'duplicate-line)))

(use-package bookmark
:bind
("C-c b" . 'bookmark-bmenu-list))

(use-package whitespace
  :bind
  ("C-c w" . 'whitespace-cleanup))

;; tramp
(use-package tramp
  :defer 5
  :config
  (setq tramp-default-method "plinkx")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path "/home/mk/.cargo/bin"))

;; isearch
(use-package isearch
  :bind
  (("C-s" . 'isearch-forward-regexp)
   ("C-r" . 'replace-regexp))
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
  :bind
  ("C-c r" . 'rotate-window)
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
  :bind
  ("C-c l" . 'goto-line)
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

(defun fris/other-window-1 ()
  (interactive)
  (other-window -1))

(use-package window
  :config
  ;; resuse help buffer
  (setq display-buffer-alist '(("\*Help\*" display-buffer-reuse-window)
                               ("\*Warning\*" display-buffer-reuse-window)
                               ("\*Compile-Log\*" display-buffer-reuse-window)
                               ("\*compilation\*" display-buffer-reuse-window)))
  (setq-default pop-up-windows nil
                scroll-preserve-screen-position t)
  ;;(advice-add 'other-window :before
  ;;            (defun other-window-scroll-plit-if-single (&rest _)
  ;;              "Split the frame if there is a single window."
  ;;              (when (one-window-p) (split-window-sensibly))))
  :bind
  (("C-<tab>" . 'other-window)
   ("C-S-<tab>" . 'fris/other-window-1)))

(use-package menu-bar
  :config
  (menu-bar-mode -1))

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package scroll-bar
  :config
  (horizontal-scroll-bar-mode -1)
  (scroll-bar-mode -1))

(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode t)
  (defun fris/disable-display-line-numbers-mode ()
    (interactive)
    (display-line-numbers-mode -1))
  :hook ((org-mode . fris/disable-display-line-numbers-mode)
         (dired-mode . fris/disable-display-line-numbers-mode)
         (term-mode . fris/disable-display-line-numbers-mode)
         (buffer-list-update . fris/disable-display-line-numbers-mode)
         (help-mode . fris/disable-display-line-numbers-mode)
         (eshell-mode . fris/disable-display-line-numbers-mode)))

;; so-long
(use-package so-long
  :config
  (global-so-long-mode 1))

;; hl-line
(use-package hl-line
  :config
  (global-hl-line-mode t))

;; saveplace
(use-package saveplace
  :config
  (save-place-mode nil))

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

;;(use-package time
;;  :config
;;  (display-time-mode t)
;;  (setq-default display-time-format "%a %d %b %H:%M"
;;                display-time-default-load-average nil
;;                display-time-load-average ""))

(use-package ido
  :config
  (ido-mode t)
  (ido-everywhere t))

(use-package prog-mode
  :hook
  ((prog-mode . hs-minor-mode)
   (prog-mode . display-line-numbers-mode)))

(use-package hideshow
  :bind
  (("C-c C-l" . 'hs-toggle-hiding)
   ("C-c C-k" . 'hs-hide-all)
   ("C-c C-j" . 'hs-show-all))
  :config
  (setq hs-hide-comments nil
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
  (hs-minor-mode)
  (dolist
      (prog-modes '(c-mode-common-hook
                    emacs-lisp-mode-hook
                    rust-mode-hook
                    c++-mode-hook
                    js-mode-hook
                    java-mode-hook))
    (add-hook prog-modes 'hs-minor-mode)))

(use-package ibuffer
  :bind
  (("M-}" . 'ibuffer)
   ("C-x C-b" . 'ibuffer))
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

;;(load-file "~/.emacs.d/lisp/hideshowvis.el")
;;(use-package hideshowvis
;;  :defer 5
;;  :config
;;  (hideshowvis-minor-mode))

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
  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
  :bind
  ("C-c c" . 'crux-cleanup-buffer-or-region))

(use-package editorconfig
  :ensure t
  :defer 1
  :config
  (editorconfig-mode 1))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

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

;;(use-package undo-tree
;;  :ensure t
;;  :config
;;  (global-undo-tree-mode)
;;  (setq undo-tree-enable-undo-in-region nil
;;        undo-tree-auto-save-history nil)
;;  )
;; ==============================================================================

;; Modeline2 --------------------------------------------------------------------
(setq-default
 mode-line-format
 '(:eval
   '("%e"
     mode-line-front-space
     mode-line-mule-info
     mode-line-client
     mode-line-modified
     mode-line-remote
     " "
     (:eval
      (if (mode-line-window-selected-p)
          (propertize " %b "
                      'face '(:weight bold)
                      'local-map '(keymap
                                  (mode-line keymap
                                             (mouse-3 . mode-line-previous-buffer)
                                             (mouse-1 . mode-line-next-buffer)))
                      'mouse-face 'mode-line-highlight)
        " %b "))
     " "
     mode-line-percent-position
     mode-line-position-column-line-format
     (vc-mode vc-mode)
     " "
     (:eval
      (propertize (replace-regexp-in-string "-mode" "" (concat " " (symbol-name major-mode) " "))
                  'face '(:weight bold)))
     mode-line-misc-info)))

;; remove box attribute from modeline
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

;; ==============================================================================

(setq gc-cons-threshold 800000)
