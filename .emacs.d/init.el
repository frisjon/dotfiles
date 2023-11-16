;; -*- lexical-binding: t; -*-

(make-directory "~/.emacs.d/config/" t)
(push "~/.emacs.d/config" load-path)

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

(load-file "~/.emacs.d/config/settings.el")
(load-file "~/.emacs.d/lisp/functions.el")
(load-file "~/.emacs.d/config/packages.el")
(load-file "~/.emacs.d/config/bindings.el")
(load-file "~/.emacs.d/feeds.el")

