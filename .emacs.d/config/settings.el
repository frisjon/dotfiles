(setq default-directory "~/")

(make-directory "~/.emacs.d/backups/" t)
(make-directory "~/.emacs.d/autosave/" t)
(make-directory "~/.emacs.d/lisp/" t)
;;(make-directory "~/.config/emacs/savehist" t)

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
(setq-default tab-width 4)
(setq-default show-trailing-whitespace t)
(dolist (mode '(dired-mode-hook
                 term-mode-hook
                 help-mode-hook
                 messages-buffer-mode-hook
                 eshell-mode-hook))
  (add-hook mode (lambda () (setq show-trailing-whitespace -1))))

;;https://emacs.stackexchange.com/questions/28906/how-to-switch-off-the-sounds
(setq ring-bell-function 'ignore)

;;https://www.emacswiki.org/emacs/YesOrNoP
(defalias 'yes-or-no-p 'y-or-n-p)
(fset 'yes-or-no-p 'y-or-n-p)

;;eshell aliases
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

;;https://emacs.stackexchange.com/questions/29096/how-to-sort-directories-first-in-dired
(add-hook 'emacs-startup-hook 'fris/emacs-startup-time)

;;(require 'ls-lisp)
(setq-default
  ls-lisp-dirs-first t
  ls-lisp-use-insert-directory-program nil)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;;(require 'rotate)
(setq rotate-functions '(rotate:main-vertical))

;;(require 'ido)
(ido-mode t)

;;(require 'windmove)
(windmove-mode t)

;;http://xahlee.info/emacs/emacs/emacs_dired_tips.html
;;(require 'dired)
(if (< emacs-major-version 28)
  (progn
    ;; was dired-advertised-find-file
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
    ;; was dired-up-directory
    (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))
  (setq dired-kill-when-opening-new-dired-buffer t))

(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;;(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;(require 'project)

;;(require 'electric)
(electric-indent-mode t)

;;(require 'delsel)
(delete-selection-mode t)

;;(require 'elec-pair)
(electric-pair-mode t)
(setq electric-pair-pairs
  '((?\" . ?\")
     (?\' . ?\')
     (?< . ?>)
     (?\{ . ?\})))

;;(require 'paren)
(show-paren-mode t)

;;(require 'simple)
(column-number-mode)
(setq-default indent-tabs-mode nil)

;;(require 'cc-vars)
(setq-default c-basic-offset 4)

;;(require 'org)
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

;;(require 'tab-bar)
(setq-default
  tab-bar-close-button-show nil
  tab-bar-tab-hints t)

;;(require 'window)
(setq-default pop-up-windows nil)

;;(require 'menu-bar)
(menu-bar-mode -1)

;;(require 'tool-bar)
(tool-bar-mode -1)

;;(require 'scroll-bar)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;;(require 'display-line-numbers)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                 dired-mode-hook
                 term-mode-hook
                 buffer-list-update-hook
                 help-mode-hook
                 eshell-mode-hook))
  (add-hook mode #'(lambda () (display-line-numbers-mode -1))))

;;(require 'hl-line)
(global-hl-line-mode t)

;;(require 'saveplace)
(save-place-mode t)

;;(require 'savehist)
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode t)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; update buffers from disk
;;(require 'autorevert)
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)

;; Dont warn for following symlinked files
;;(require 'vc-hooks)
(setq vc-follow-symlinks t)

;;(require 'time)
(display-time-mode t)
(setq-default display-time-format "%a %d %b %H:%M"
  display-time-default-load-average nil
  display-time-load-average "")

;;(require 'erc)
;;(setq erc-nick "fris"
;;erc-system-name "frispc")

;;(require 'recentf)
;;(recentf-mode t)

;; themes
(push "~/.emacs.d/themes" load-path)
(load-file "~/.emacs.d/themes/ef-themes.el")
(when 'ef-kassio
  (ef-themes-select 'ef-kassio))

(setq dired-clean-confirm-killing-deleted-buffers nil
  dired-confirm-shell-command nil
  dired-no-confirm t
  dired-recursive-deletes 'always
  dired-deletion-confirmer '(lambda (x) t))

(add-hook 'kill-buffer-query-functions (lambda () (not-modified) t))
