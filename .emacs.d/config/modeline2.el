(setq-default
 mode-line-format
 '(:eval
   '("%e"
     mode-line-front-space
     mode-line-mule-info
     mode-line-client
     mode-line-modified
     mode-line-remote
     " "
     (:eval
      (if (mode-line-window-selected-p)
          (propertize " %b "
                      'face '(:weight bold)
                      'local-map '(keymap
                                  (mode-line keymap
                                             (mouse-3 . mode-line-previous-buffer)
                                             (mouse-1 . mode-line-next-buffer)))
                      'mouse-face 'mode-line-highlight)
        " %b "))
     " "
     mode-line-percent-position
     mode-line-position-column-line-format
     (vc-mode vc-mode)
     " "
     (:eval
      (propertize (replace-regexp-in-string "-mode" "" (concat " " (symbol-name major-mode) " "))
                  'face '(:weight bold)))
     mode-line-misc-info)))
