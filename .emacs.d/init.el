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
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(load-file "~/.emacs.d/config/settings.el")
(load-file "~/.emacs.d/config/functions.el")
(load-file "~/.emacs.d/config/bindings.el")
(load-file "~/.emacs.d/config/packages.el")
(load-file "~/.emacs.d/config/modeline2.el")

(fris/remove-box-attr-from-modeline)

;;(load-file "~/.emacs.d/feeds.el")

(setq gc-cons-threshold 800000)
