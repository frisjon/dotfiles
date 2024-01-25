;;(set-face-attribute
;; 'mode-line nil
;; :background (fris/color "")
;; :foreground (fris/color "")
;; :box (modus-themes-color 'bg-main))

(when 'ef-themes-with-colors
  (progn
    (defface fris-modeline--major-mode-face
      `((t :background ,(ef-themes-with-colors bg-red-intense)
           :foreground ,(ef-themes-with-colors fg-mode-line)))
      "Face for custom modeline. Used for major mode in active modeline")

    (defface fris-modeline--major-mode-face-inactive
      `((t :background ,(ef-themes-with-colors bg-tab-bar)
           :foreground ,(ef-themes-with-colors fg-mode-line)))
      "Face for custom modeline. Used for major mode in inactive modeline")

    (defface fris-modeline--buffer-name-face
      `((t :background ,(ef-themes-with-colors bg-green-intense)
           :foreground ,(ef-themes-with-colors fg-mode-line)))
      "Face for custom modeline. Used for buffer name in active modeline")

    (defface fris-modeline--buffer-name-face-inactive
      `((t :background ,(ef-themes-with-colors bg-tab-bar)
           :foreground ,(ef-themes-with-colors fg-mode-line)))
      "Face for custom modeline. Used for buffer name in inactive modeline")

    (defface fris-modeline--mule-face
      `((t :background ,(ef-themes-with-colors bg-blue-intense)
           :foreground ,(ef-themes-with-colors fg-mode-line)))
      "Face for custom modeline. Used for mule info")

    (defun fris-modeline/major-mode-string ()
      "Return string with major mode. To be used in custom modeline"
      (format " %s " (capitalize (symbol-name major-mode))))

    (defvar-local fris-modeline--major-mode
        '(:eval (if (mode-line-window-selected-p)
                    (propertize (fris-modeline/major-mode-string) 'face
                                'fris-modeline--major-mode-face)
                  (propertize (fris-modeline/major-mode-string) 'face
                              'fris-modeline--major-mode-face-inactive)))
      "Local variable to show major mode. To be used in custom modeline")

    (defun fris-modeline/buffer-name-string ()
      "Return string with buffer name. To be used in custom modeline"
      (format " %s "(buffer-name)))

    (defvar-local fris-modeline--buffer-name
        '(:eval
          (if (mode-line-window-selected-p)
              (propertize
               (fris-modeline/buffer-name-string)
               'face 'fris-modeline--buffer-name-face)
            (propertize
             (fris-modeline/buffer-name-string)
             'face 'fris-modeline--buffer-name-face-inactive)))
      "Local variable to show buffer name. To be used in custom modeline")

    (defvar-local fris-modeline--time
        '(:eval
          (when (mode-line-window-selected-p) global-mode-string))
      "Local variable to show time. To be used in custom modeline")

    (defun fris-modeline/buffer-coding-system-string (encoding)
      "Return string showing buffer encoding. To be used in custom modeline"
      (cond ((string= encoding "utf-8-unix") "UTF-8")
            (t encoding)))

    (defun fris-modeline/eol-style-string (eol)
      "Return string showing eol style. To be used in custom modeline"
      (cond ((= eol 0) "LF")
            ((= eol 1) "CRLF")
            ((= eol 2) "CR")
            (t "")))

    (defun fris-modeline/mule-string ()
      "Return string to display in modeline. Shows buffer encoding and eol style"
      (let ((encoding (symbol-name buffer-file-coding-system))
            (eol (coding-system-eol-type buffer-file-coding-system)))
        (format " %s %s "
                (fris-modeline/buffer-coding-system-string encoding)
                (fris-modeline/eol-style-string eol))))

    (defvar-local fris-modeline--mule
        '(:eval
          (when (mode-line-window-selected-p)
            (propertize (fris-modeline/mule-string)
                        'face 'fris-modeline--mule-face)))
      "Local variable to mule info. To be used in custom modeline")

    ;; add custom variables
    (dolist (var '(fris-modeline--buffer-name
                   fris-modeline--major-mode
                   fris-modeline--time
                   fris-modeline--mule))
      (put var 'risky-local-variable t))

    ;; set modeline
    (setq-default
     mode-line-format
     `("%e "
       mode-line-modified
       mode-line-remote
       fris-modeline--buffer-name
       fris-modeline--major-mode
       fris-modeline--mule
       ;;mode-line-front-space
       " (%l,%C) %I"
       " "
       (vc-mode vc-mode)
       fris-modeline--time
       ))



    ))
