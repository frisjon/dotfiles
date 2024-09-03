(setq
 mode-line-format
 '(:eval
   '("%e"
     mode-line-front-space
     mode-line-mule-info
     mode-line-client
     mode-line-modified
     mode-line-remote
     (" ["
      (:eval
       (if (mode-line-window-selected-p)
           (propertize "%b" 'face '(:background "red" :foreground "yellow"))
        "%b")) "]")
     (" <"
      mode-line-percent-position
      mode-line-position-column-line-format
      ">")
     (vc-mode vc-mode)
     " "
     (:eval (propertize (replace-regexp-in-string "-mode" "" (symbol-name major-mode)) 'face '(:background "red")))

     mode-line-misc-info
     )))


