(setq-default
 mode-line-format
   '("%e"
     mode-line-front-space
     mode-line-mule-info
     mode-line-client
     mode-line-modified
     mode-line-remote
     (" ["
      (:eval
       (if (mode-line-window-selected-p)
           (propertize "%b" 'face '(:background "red" :foreground "green"))
        "%b")) "]")
     (" <"
      mode-line-percent-position
      mode-line-position-column-line-format
      ">")
     (vc-mode vc-mode)
     (" ("
      (:eval (propertize mode-name 'face '(:background "red" :foreground "blue")))
      ") ")
     mode-line-misc-info
     ))
