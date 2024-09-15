;; -*- lexical-binding: t; -*-
(defun fris/log (msg)
  (interactive)
  (message (concat (format-time-string "%Y-%m-%d %H:%M:%S.%3N") ": " msg))
  )

(fris/log "Init")
;;(setq use-package-compute-statistics t)
(make-directory "~/.emacs.d/config/" t)
(push "~/.emacs.d/config" load-path)

;; Initialize package sources
;;(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;;(package-initialize)
;;(unless package-archive-contents
;; (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
;;(unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

(fris/log "before use-package")
(require 'use-package)
(fris/log "Loading settings.el")
(load-file "~/.emacs.d/config/settings.el")
(fris/log "Loading functions.el")
(load-file "~/.emacs.d/config/functions_binding.el")
(fris/log "Loading packages.el")
(load-file "~/.emacs.d/config/packages.el")
(fris/log "Loading modeline.el")
(load-file "~/.emacs.d/config/modeline2.el")
(fris/log "mod face")
(fris/remove-box-attr-from-modeline)
;;(load-file "~/.emacs.d/feeds.el")
(fris/log "End")
