(make-directory "~/.emacs.d/backups/" t)
(make-directory "~/.emacs.d/autosave/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
;;(setq auto-save-default nil)

(make-directory "~/.emacs.d/themes/" t)
(make-directory "~/.emacs.d/lisp/" t)
(push "~/.emacs.d/lisp" load-path)
(push "~/.emacs.d/themes" load-path)

(setq default-directory "~/")

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

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
;; requires font source code pro to be installed
(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))
(when (require 'sanity nil 'noerror))
(require 'ef-themes)
(load-theme 'ef-trio-dark t)

;; command history
(setq history-length 25)
(savehist-mode 1)

;; supress dialog box
(setq use-dialog-box nil)

;; update buffers from disk
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Dont warn for following symlinked files
(setq vc-follow-symlinks t)

(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)

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

;;https://emacs.stackexchange.com/questions/28906/how-to-switch-off-the-sounds
(setq ring-bell-function 'ignore)

(put 'dired-find-alternate-file 'disabled nil)

;;https://www.emacswiki.org/emacs/YesOrNoP
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'l 'eshell/ls)
(defalias 'll (lambda () (eshell/ls "-l" )))
(defalias 'la (lambda () (eshell/ls "-la")))
(defalias 'less (lambda (a) (view-file a)))

(setq org-support-shift-select t)

(setq delete-by-moving-to-trash t)

;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;;(setq scroll-step 1) ;; keyboard scroll one line at a time

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
