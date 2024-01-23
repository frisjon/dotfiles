(make-directory "~/.emacs.d/backups/" t)
(make-directory "~/.emacs.d/autosave/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
;;(setq auto-save-default nil)

;;(make-directory "~/.emacs.d/themes/" t)
(make-directory "~/.emacs.d/lisp/" t)
(push "~/.emacs.d/lisp" load-path)
;;(push "~/.emacs.d/themes" load-path)

(setq default-directory "~/")

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
;;(setq gc-cons-threshold (* 100 1024 1024))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      initial-scratch-message nil)
(setq-default tab-always-indent 'complete)
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8)
(set-language-environment "UTF-8")

;; move customization to file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(setq visible-bell nil)

;; theme
;; requires font source code pro to be installed
(add-to-list 'default-frame-alist '(font . "Noto Sans Mono-12"))
;;(when (require 'sanity nil 'noerror))

;; command history
(setq history-length 25)

;; supress dialog box
(setq use-dialog-box nil)

;;(whitespace-mode t)
(setq-default show-trailing-whitespace t)
(setq-default tab-width 4)

;;https://emacs.stackexchange.com/questions/28906/how-to-switch-off-the-sounds
(setq ring-bell-function 'ignore)

(put 'dired-find-alternate-file 'disabled nil)

;;https://www.emacswiki.org/emacs/YesOrNoP
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'l 'eshell/ls)
(defalias 'll (lambda () (eshell/ls "-l" )))
(defalias 'la (lambda () (eshell/ls "-la")))
(defalias 'less (lambda (a) (view-file a)))

(setq delete-by-moving-to-trash t)

;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;;(setq scroll-step 1) ;; keyboard scroll one line at a time

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

(setq fill-column 80)

;; focus help window
(setq-default help-window-select t)

;; resuse help buffer
(setq display-buffer-alist '(("\*Help\*" display-buffer-reuse-window)))

;;(setq
;; mode-line-format
;; `("%e"
;;   ;;(:eval (propertize (if (buffer-modified-p) " *" " ") 'face 'hl-line))
;;   (:eval (propertize (" " mode-line-mule-info mode-line-client mode-line-modified mode-line-remote) 'face 'hl-line))
;;   (:eval (propertize (format " %s " (buffer-name)) 'face 'hl-line))
;;   (:eval (propertize (format " %s " (capitalize (symbol-name major-mode))) 'face 'holiday))
;;   ;;mode-line-front-space
;;   " (%l,%C) %I"
;;   " "
;;   (vc-mode vc-mode)
;;   mode-line-misc-info
;;   mode-line-end-spaces
;;   ))
