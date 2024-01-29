;; modeline

(when 'ef-themes-with-colors
  (defun fris-modeline/reeval-faces-on-theme-change ()
    (interactive)
    (set-face-attribute 'fris-modeline--major-mode-face nil
      :background (ef-themes-with-colors bg-red-intense)
      :foreground (ef-themes-with-colors fg-mode-line))

    (set-face-attribute 'fris-modeline--major-mode-face-inactive nil
      :background (ef-themes-with-colors bg-tab-bar)
      :foreground (ef-themes-with-colors fg-mode-line))

    (set-face-attribute 'fris-modeline--buffer-name-face nil
      :background (ef-themes-with-colors bg-green-intense)
      :foreground (ef-themes-with-colors fg-mode-line))

    (set-face-attribute 'fris-modeline--buffer-name-face-inactive nil
      :background (ef-themes-with-colors bg-tab-bar)
      :foreground (ef-themes-with-colors fg-mode-line))

    (set-face-attribute 'fris-modeline--mule-face nil
      :background (ef-themes-with-colors bg-blue-intense)
      :foreground (ef-themes-with-colors fg-mode-line))))

(when 'ef-themes-with-colors
  (progn
    ;; Faces ------------------------------------------------------------------
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

    ;; Functions --------------------------------------------------------------
    (defun fris-modeline/major-mode-string ()
      "Return string with major mode. To be used in custom modeline"
      (format " %s " (capitalize (symbol-name major-mode))))

    (defun fris-modeline/buffer-name-string ()
      "Return string with buffer name. To be used in custom modeline"
      (format " %s "(buffer-name)))

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

    (defun fris-modeline/buffer-read-only-string()
      "Return string showing if buffer is read-only.
To be used in custom modeline"
      (if (buffer-local-value 'buffer-read-only (current-buffer))
          "r"
          "w"))

    (defun fris-modeline/buffer-modified-string()
      "Return string showing if buffer was modified"
      (if (buffer-modified-p)
          "*"
          "-"))

    (defun fris-modeline/buffer-coding-system-string (encoding)
      "Return string showing buffer encoding. To be used in custom modeline"
      (cond ((string= encoding "utf-8-unix") "UTF-8")
            (t encoding)))

    (defun fris-modeline/toggle-read-only ()
      (interactive)
      (with-selected-window (selected-window)
        (read-only-mode 'toggle)))

    (defun fris-modeline/toggle-buffer-modified ()
      (interactive)
      (with-selected-window (selected-window)
        (set-buffer-modified-p (not (buffer-modified-p)))
        (force-mode-line-update)))

    (defun fris-modeline/switch-eol ()
      (interactive)
      (mode-line-change-eol
       (make-sparse-keymap)))

    ;; Variables --------------------------------------------------------------
    (defvar-local fris-modeline--major-mode
      '(:eval (if (mode-line-window-selected-p)
                (propertize (fris-modeline/major-mode-string)
                  'face 'fris-modeline--major-mode-face
                  'local-map '(keymap (mode-line keymap
                                        (mouse-3 menu-item "menu bar")
                                        (mouse-1 . describe-mode)))
                  )
                (propertize (fris-modeline/major-mode-string) 'face
                  'fris-modeline--major-mode-face-inactive)))
      "Local variable to show major mode. To be used in custom modeline")

    (defvar-local fris-modeline--buffer-name
      '(:eval
         (if (mode-line-window-selected-p)
           (propertize
             (fris-modeline/buffer-name-string)
             'face 'fris-modeline--buffer-name-face
             'help-echo " Buffer name"
             'local-map '(keymap (mode-line keymap
                                   (mouse-3 . previous-buffer)
                                   (mouse-1 . next-buffer))))
           (propertize
             (fris-modeline/buffer-name-string)
             'face 'fris-modeline--buffer-name-face-inactive)))
      "Local variable to show buffer name. To be used in custom modeline")

    (defvar-local fris-modeline--time
      '(:eval
         (when (mode-line-window-selected-p) global-mode-string))
      "Local variable to show time. To be used in custom modeline")

    (defvar-local fris-modeline--mule
      '(:eval
         (when (mode-line-window-selected-p)
           (propertize (fris-modeline/mule-string)
             'face 'fris-modeline--mule-face
             'local-map '(keymap (mode-line keymap
                                   (mouse-1 . fris-modeline/switch-eol))))))
      "Local variable to mule info. To be used in custom modeline")

    (defvar-local fris-modeline--buffer-read-only
        '(:eval
          (propertize
           (fris-modeline/buffer-read-only-string)
           'face 'bold
           'local-map '(keymap
                        (mode-line keymap
                                   (mouse-1 . fris-modeline/toggle-read-only)))))
      "")

    (defvar-local fris-modeline--buffer-modified
        '(:eval
          (propertize (fris-modeline/buffer-modified-string)
                      'face 'bold
                      'local-map '(keymap
                        (mode-line keymap
                                   (mouse-1 . fris-modeline/toggle-buffer-modified)))
                      ))
      "")

    ;; Add custom variables ---------------------------------------------------
    (dolist (var '(fris-modeline--buffer-name
                    fris-modeline--major-mode
                    fris-modeline--time
                    fris-modeline--mule
                    fris-modeline--buffer-modified
                    fris-modeline--buffer-read-only))
      (put var 'risky-local-variable t))

    ;; Modeline ---------------------------------------------------------------
    (setq-default
      mode-line-format
      '(:eval
         (fris/simple-mode-line-render
           ;; left
           (format-mode-line
            '("%e"
               fris-modeline--buffer-read-only
               fris-modeline--buffer-modified
               mode-line-remote " "
                fris-modeline--buffer-name fris-modeline--major-mode
                " (%l,%C) %I"))
           ;; right
           (format-mode-line
             '("" fris-modeline--mule (vc-mode vc-mode)
                " " fris-modeline--time)))))

    ;; Hook to update faces in modeline ---------------------------------------
    (add-hook 'ef-themes-post-load-hook
      'fris-modeline/reeval-faces-on-theme-change)
    ;; warning. the hook is used by ef-themes-select. dont use load-theme
    ))
