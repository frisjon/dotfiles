;; -*- lexical-binding: t; -*-
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
(global-set-key (kbd "M-}") 'ibuffer)
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

;;(require 'misc)
(global-set-key (kbd "C-S-d") 'duplicate-line)
(global-set-key (kbd "C-d") 'duplicate-line)

;;(require 'bookmark)
(global-set-key (kbd "C-c b") 'bookmark-bmenu-list)

;;(require 'crux)
(global-set-key (kbd "C-c c") 'crux-cleanup-buffer-or-region)

;;(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;(require 'whitespace)
(global-set-key (kbd "C-c w") 'whitespace-cleanup)

;;(require 'isearch)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'replace-regexp)

;;(require 'window)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-S-<tab>") #'(lambda () (interactive) (other-window -1)))

;;(require 'rotate)
(global-set-key (kbd "C-c r") 'rotate-window)

;;(require 'simple)
(global-set-key (kbd "C-c l") 'goto-line)

(global-set-key (kbd "C-c C-l") 'hs-toggle-hiding)
(global-set-key (kbd "C-c C-k") 'hs-hide-all)
(global-set-key (kbd "C-c C-j") 'hs-show-all)
(eval-after-load 'c-mode '(define-key c-mode-map (kbd "C-c C-l") 'hs-toggle-hiding))
(eval-after-load 'c-mode '(define-key c-mode-map (kbd "C-c C-k") 'hs-hide-all))
(eval-after-load 'c-mode '(define-key c-mode-map (kbd "C-c C-j") 'hs-show-all))
;;(define-key c-mode-map (kbd "C-c C-l") 'hs-toggle-hiding)
;;(define-key c-mode-map (kbd "C-c C-k") 'hs-hide-all)
;;(define-key c-mode-map (kbd "C-c C-j") 'hs-show-all)

(global-set-key (kbd "C-S-s") 'highlight-regexp)

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
