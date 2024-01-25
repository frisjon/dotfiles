;;https://emacs.stackexchange.com/questions/29096/how-to-sort-directories-first-in-dired
(add-hook 'emacs-startup-hook 'fris/emacs-startup-time)

;;(require 'ls-lisp)
(setq-default
  ls-lisp-dirs-first t
  ls-lisp-use-insert-directory-program nil)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;;(require 'rotate)
(setq rotate-functions '(rotate:main-vertical))
(global-set-key (kbd "C-c r") 'rotate-window)

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
(global-set-key (kbd "C-c l") 'goto-line)
;; ("C-c l" . goto-line)

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
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-S-<tab>") '(lambda () (interactive) (other-window -1)))

;;(require 'isearch)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'replace-regexp)

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
                 term-mode-hook
                 help-mode-hook
                 eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;(require 'hl-line)
(global-hl-line-mode 1)

;;(require 'saveplace)
(save-place-mode 1)

;;(require 'savehist)
(savehist-mode 1)

;; update buffers from disk
;;(require 'autorevert)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Dont warn for following symlinked files
;;(require 'vc-hooks)
(setq vc-follow-symlinks t)

;;(require 'time)
(display-time-mode t)
(setq-default display-time-format "%a %d %b %H:%M"
  display-time-default-load-average nil
  display-time-load-average "")

;;(require 'misc)
(global-set-key (kbd "C-S-d") 'duplicate-line)
(global-set-key (kbd "C-D") 'duplicate-line)

;;(require 'bookmark)
(global-set-key (kbd "C-c b") 'bookmark-bmenu-list)

;;(require 'crux)
(global-set-key (kbd "C-c c") 'crux-cleanup-buffer-or-region)

;;(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;(require 'whitespace)
(global-set-key (kbd "C-c w") 'whitespace-cleanup)

;;(require 'erc)
;;(setq erc-nick "fris"
;;erc-system-name "frispc")

(use-package edwina
  :ensure t
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  (edwina-setup-dwm-keys)
  (edwina-mode 1)
  (defun edwina-mode-line-indicator ()
    "redefining this func" ""))

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
  :hook
  (after-init . global-company-mode))

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

(use-package elfeed
  :ensure t
  :config
  (defun elfeed-search-browse-all-url ()
    (interactive)
    (if (< (count-lines (point-min) (point-max)) 20)
        (progn
          (mark-whole-buffer)
          (elfeed-search-browse-url))
      (message "More than 20 links to open. Not proceeding")))
  (define-key elfeed-search-mode-map "B" 'elfeed-search-browse-all-url)
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
  :config
  (editorconfig-mode 1))
