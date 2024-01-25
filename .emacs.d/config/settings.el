(setq default-directory "~/.emacs.d")

(make-directory "~/.emacs.d/backups/" t)
(make-directory "~/.emacs.d/autosave/" t)
(make-directory "~/.emacs.d/lisp/" t)

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t))
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      backup-by-copying t)
;;(setq auto-save-default nil)

(push "~/.emacs.d/lisp" load-path)

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      initial-scratch-message nil)
(setq-default tab-always-indent 'complete)
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)
(setq coding-system-for-read 'utf-8-unix)
(setq coding-system-for-write 'utf-8-unix)

;; move customization to file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(setq visible-bell nil)

;; font
;;(add-to-list 'default-frame-alist '(font . "Noto Sans Mono-12"))
;; https://www.youtube.com/watch?v=qR8JRYr4BKE
(set-face-attribute 'default nil
                    :family "Noto Sans Mono"
                    :height 120)
(set-face-attribute 'fixed-pitch nil
                    :family "Noto Sans Mono"
                    :height 120)

;; command history
(setq history-length 25)

;; supress dialog box
(setq use-dialog-box nil)

;;(whitespace-mode t)
(setq-default show-trailing-whitespace t)
(setq-default tab-width 4)

;;https://emacs.stackexchange.com/questions/28906/how-to-switch-off-the-sounds
(setq ring-bell-function 'ignore)

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

(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

(setq fill-column 80)

;; focus help window
(setq-default help-window-select t)

;; resuse help buffer
(setq display-buffer-alist '(("\*Help\*" display-buffer-reuse-window)))

(setq system-time-locale "C")
(setq system-name "jon")

;; themes
(push "~/.emacs.d/themes" load-path)
(load-file "~/.emacs.d/themes/ef-themes.el")
(when 'ef-kassio
  (ef-themes-select 'ef-kassio))

