;; -*- lexical-binding: t; -*-
(setq default-directory "~/")

(make-directory "~/.emacs.d/backups/" t)
(make-directory "~/.emacs.d/autosave/" t)
(make-directory "~/.emacs.d/lisp/" t)
;;(make-directory "~/.config/emacs/savehist" t)

(push "~/.emacs.d/lisp" load-path)

;;(setq auto-save-default nil)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t))
	  backup-directory-alist '(("." . "~/.emacs.d/backups"))
	  backup-by-copying t
      gc-cons-threshold (* 2 1000 1000) ;; The default is 800 kilobytes.  Measured in bytes.
      inhibit-startup-screen t
	  inhibit-startup-echo-area-message t
	  inhibit-startup-message t
	  initial-scratch-message nil
      coding-system-for-read 'utf-8-unix
      coding-system-for-write 'utf-8-unix
      custom-file (locate-user-emacs-file "custom-vars.el") ;; move customization to file
      visible-bell nil
      history-length 25 ;; command history
      use-dialog-box nil ;; supress dialog box
      ;;https://emacs.stackexchange.com/questions/28906/how-to-switch-off-the-sounds
      ring-bell-function 'ignore
      delete-by-moving-to-trash t
      read-buffer-completion-ignore-case t
      fill-column 80
      explicit-shell-file-name "/bin/bash"
      display-buffer-alist '(("\*Help\*" display-buffer-reuse-window)) ;; resuse help buffer
      system-time-locale "C"
      system-name "jon")

(setq-default tab-width 4
              show-trailing-whitespace t
              help-window-select t ;; focus help window
              tab-always-indent 'complete ;; coding system
              buffer-file-coding-system 'utf-8-unix) ;; coding system

(set-default-coding-systems 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)

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
 (cond
  ((eq system-type 'windows-nt)
   (cond
    ((member "Segoe UI Symbol" (font-family-list)) "Segoe UI Symbol")
    ((member "all-the-icons" (font-family-list)) "all-the-icons")
    ))
  ((eq system-type 'gnu/linux)
   (cond
    ((member "Symbola" (font-family-list)) "Symbola")))))

(progn
  (set-fontset-font
   t
   'emoji
   (cond
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ((member "all-the-icons" (font-family-list)) "all-the-icons")
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Symbola" (font-family-list)) "Symbola"))))

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

;;eshell aliases
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
(add-to-list 'exec-path (expand-file-name "/plinkx:local_wsl:~/.cargo/bin/"))
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))

;;https://emacs.stackexchange.com/questions/29096/how-to-sort-directories-first-in-dired
(add-hook 'emacs-startup-hook 'fris/emacs-startup-time)
(add-hook 'kill-buffer-query-functions (lambda () (not-modified) t))

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(add-hook 'after-load-theme-hook
  (lambda ()
    (progn
      ;; remove box attribute from modeline
      (set-face-attribute 'mode-line nil :box nil)
      (set-face-attribute 'mode-line-inactive nil :box nil))))

