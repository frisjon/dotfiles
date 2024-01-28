(defun fris/emacs-startup-time ()
    "Profile emacs startup"
    (message "*** Emacs loaded in %s seconds with %d garbage collections."
             (emacs-init-time "%.2f")
             gcs-done))

(defun fris/backward-kill-char-or-word ()
  "To replace default backward-kill-word
Taken from https://emacs.stackexchange.com/questions/30401/backward-kill-word-kills-too-much-how-to-make-it-more-intelligent"
  (interactive)
  (cond ((looking-back (rx (char word)) 1)
         (backward-kill-word 1))
        ((looking-back (rx (char blank)) 1)
         (delete-horizontal-space t))
        (t (backward-delete-char 1))))

(defun fris/kill-this-buffer ()
  (interactive) (kill-buffer (current-buffer)))

(defun fris/xah-new-empty-buffer ()
  "Create a new empty buffer.
Returns the buffer object.
New buffer is named untitled, untitled<2>, etc.

Warning: new buffer is not prompted for save when killed, see `kill-buffer'.
Or manually `save-buffer'

URL `http://xahlee.info/emacs/emacs/emacs_new_empty_buffer.html'
Version: 2017-11-01 2022-04-05"
  (interactive)
  (let ((xbuf (generate-new-buffer "untitled")))
    (switch-to-buffer xbuf)
    (funcall initial-major-mode)
    xbuf))

(defun fris/edwina-focus-master ()
  "select window with biggest area (presumably master)"
  (interactive)
  (if (window-left-child(frame-root-window))
    (select-window(window-left-child(frame-root-window)))
  (select-window (frame-root-window))))

(defun fris/edwina-new-empty-buffer-in-window ()
  "open new empty buffer in new windows to the left. dwm style"
  (interactive)
  (fris/edwina-focus-master)
  (edwina-clone-window)
  (fris/xah-new-empty-buffer))

(defun fris/find-file-wsl ()
  "find-file in remote location.
Use on windows. Requires to have putty installed, with a remote location saved as local_wsl.
In this case, the remote location is WSL running on windows. WSL must have ssh installed and running."
  (interactive)
  (find-file "/plinkx:local_wsl:~/"))

(defun fris/edwina-kill-buffer-and-window ()
  (interactive)
  (kill-buffer-and-window)
  (edwina-arrange)
  (fris/edwina-focus-master))

(defun fris/edwina-open-eshell-in-new-window()
  (interactive)
  (edwina-clone-window)
  (eshell))

(defun fris/edwina-zoom ()
  (interactive)
  (edwina-zoom)
  (fris/edwina-focus-master))

(defun fris/edwina-delete-window()
  (interactive)
  (edwina-delete-window)
  (fris/edwina-focus-master))

(defun fris/color-name-string-to-hex-string (color)
  "Takes in string color-name variable and returns its hex code as a string"
  (apply 'format "#%02x%02x%02x"
		 (mapcar (lambda (c) (ash c -8))
				 (color-values color))))

(defalias 'fris/color 'fris/color-name-string-to-hex-string)

;; write a function to do the spacing
(defun fris/simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively.
https://emacs.stackexchange.com/questions/5529/how-to-right-align-some-items-in-the-modeline"
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(defun fris/unfill-paragraph ()
  "Taken from
https://www.reddit.com/r/emacs/comments/1act3md/how_to_replace_inparagraph_newlines_etc_with/"
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil)))
;;

(global-set-key (kbd "M-z") 'fris/edwina-focus-master)
(global-set-key [remap edwina-zoom] #'fris/edwina-zoom)
(global-set-key [remap edwina-delete-window] #'fris/edwina-delete-window)
(global-set-key (kbd "M-q") 'fris/edwina-delete-window)
(global-set-key (kbd "M-S-a") 'fris/xah-new-empty-buffer)
(global-set-key (kbd "M-A") 'fris/xah-new-empty-buffer)
(global-set-key (kbd "M-a") 'fris/edwina-new-empty-buffer-in-window)
(global-set-key (kbd "M-t") 'fris/edwina-open-eshell-in-new-window)

(global-set-key (kbd "C-<backspace>") 'fris/backward-kill-char-or-word)
(global-set-key (kbd "C-c n") 'fris/xah-new-empty-buffer)
(global-set-key (kbd "C-x k") 'fris/kill-this-buffer)
(global-set-key (kbd "C-S-k") 'fris/edwina-kill-buffer-and-window)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
;;(global-set-key (kbd "C-x C-S-f") 'fris/find-file-wsl)

(global-set-key (kbd "M-e") 'fill-paragraph)
(global-set-key (kbd "M-E") 'fris/unfill-paragraph)

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


;;(require 'isearch)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'replace-regexp)

;;(require 'window)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-S-<tab>") '(lambda () (interactive) (other-window -1)))

;;(require 'rotate)
(global-set-key (kbd "C-c r") 'rotate-window)

;;(require 'simple)
(global-set-key (kbd "C-c l") 'goto-line)

;; unsettings
;;(global-unset-key (kbd "M-{"))
;;(global-unset-key (kbd "M-}"))
(global-unset-key (kbd "<insertchar>"))
(global-unset-key (kbd "<insert>"))
(global-unset-key (kbd "C-d"))
(global-unset-key (kbd "C-S-d"))
(global-unset-key (kbd "C-x m"))

(global-unset-key (kbd "<mouse-movement>"))
(global-unset-key (kbd "<noname>"))
(global-unset-key (kbd "<rwindow>"))
(global-unset-key (kbd "<touch-end>"))
(global-unset-key (kbd "C-<noname>"))
(global-unset-key (kbd "C-M-<drag-mouse-1>"))
(global-unset-key (kbd "M-<noname>"))
(global-unset-key (kbd "<iconify-frame>"))
(global-unset-key (kbd "<language-change>"))
(global-unset-key (kbd "<lwindow>"))
(global-unset-key (kbd "<make-frame-visible>"))
(global-unset-key (kbd "C-@"))
(global-unset-key (kbd "C-x C-@"))
(global-unset-key (kbd "C-@"))
(global-unset-key (kbd "C-M-@"))
(global-unset-key (kbd "C-x r C-@"))
(global-unset-key (kbd "M-@"))
(global-unset-key (kbd "C-x @ S"))
(global-unset-key (kbd "C-x @ a"))
(global-unset-key (kbd "C-x @ c"))
(global-unset-key (kbd "C-x @ h"))
(global-unset-key (kbd "C-x @ m"))
(global-unset-key (kbd "C-x @ s"))
(global-unset-key (kbd "S-<mouse-4>"))
(global-unset-key (kbd "S-<mouse-5>"))
(global-unset-key (kbd "S-<mouse-6>"))
(global-unset-key (kbd "S-<mouse-7>"))
(global-unset-key (kbd "S-<wheel-down>"))
(global-unset-key (kbd "S-<wheel-left>"))
(global-unset-key (kbd "S-<wheel-right>"))
(global-unset-key (kbd "S-<wheel-up>"))
(global-unset-key (kbd "M-~"))
(global-unset-key (kbd "C-x v ~"))
(global-unset-key (kbd "<insert>"))
(global-unset-key (kbd "<insertchar>"))
(global-unset-key (kbd "C-<insert>"))
(global-unset-key (kbd "C-<insertchar>"))
(global-unset-key (kbd "C-M-."))
(global-unset-key (kbd "C-x 5 ."))
(global-unset-key (kbd "M-,"))
(global-unset-key (kbd "M-."))
(global-unset-key (kbd "M-?"))
(global-unset-key (kbd "C-x 4 ."))
(global-unset-key (kbd "C-M-,"))
(global-unset-key (kbd "C-d"))
(global-unset-key (kbd "<XF86Back>"))
(global-unset-key (kbd "<XF86Forward>"))
(global-unset-key (kbd "<again>"))
(global-unset-key (kbd "<begin>"))
(global-unset-key (kbd "<compose-last-chars>"))
(global-unset-key (kbd "<copy>"))
(global-unset-key (kbd "<cut>"))
(global-unset-key (kbd "<delete-frame>"))
(global-unset-key (kbd "<execute>"))
(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "<f16>"))
(global-unset-key (kbd "<f18>"))
(global-unset-key (kbd "<f20>"))
(global-unset-key (kbd "C-x ("))
(global-unset-key (kbd "C-x )"))
(global-unset-key (kbd "C-x *"))
(global-unset-key (kbd "C-x 5 0"))
(global-unset-key (kbd "C-x 5 1"))
(global-unset-key (kbd "C-x 5 2"))
(global-unset-key (kbd "C-x 5 5"))
(global-unset-key (kbd "C-x 5 C-f"))
(global-unset-key (kbd "C-x 5 C-o"))
(global-unset-key (kbd "C-x 5 b"))
(global-unset-key (kbd "C-x 5 c"))
(global-unset-key (kbd "C-x 5 d"))
(global-unset-key (kbd "C-x 5 f"))
(global-unset-key (kbd "C-x 5 m"))
(global-unset-key (kbd "C-x 5 o"))
(global-unset-key (kbd "C-x 5 p"))
(global-unset-key (kbd "C-x 5 r"))
(global-unset-key (kbd "C-x 5 u"))
(global-unset-key (kbd "C-x 8 e +"))
(global-unset-key (kbd "C-x 8 e -"))
(global-unset-key (kbd "C-x 8 e 0"))
(global-unset-key (kbd "C-x 8 e d"))
(global-unset-key (kbd "C-x 8 e e"))
(global-unset-key (kbd "C-x 8 e i"))
(global-unset-key (kbd "C-x 8 e l"))
(global-unset-key (kbd "C-x 8 e r"))
(global-unset-key (kbd "C-x 8 e s"))
(global-unset-key (kbd "C-x ="))
(global-unset-key (kbd "C-x C-<left>"))
(global-unset-key (kbd "C-x C-<right>"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-x ESC ESC"))
(global-unset-key (kbd "C-x M-:"))
(global-unset-key (kbd "C-x RET F"))
(global-unset-key (kbd "C-x RET X"))
(global-unset-key (kbd "C-x RET c"))
(global-unset-key (kbd "C-x RET f"))
(global-unset-key (kbd "C-x RET k"))
(global-unset-key (kbd "C-x RET l"))
(global-unset-key (kbd "C-x RET p"))
(global-unset-key (kbd "C-x RET r"))
(global-unset-key (kbd "C-x RET t"))
(global-unset-key (kbd "C-x RET x"))
(global-unset-key (kbd "ESC <begin>"))
(global-unset-key (kbd "ESC <end>"))
(global-unset-key (kbd "ESC <f10>"))
(global-unset-key (kbd "ESC <home>"))
(global-unset-key (kbd "ESC <left>"))
(global-unset-key (kbd "ESC <next>"))
(global-unset-key (kbd "ESC <prior>"))
(global-unset-key (kbd "ESC <right>"))
(global-unset-key (kbd "ESC C-<backspace>"))
(global-unset-key (kbd "ESC C-<delete>"))
(global-unset-key (kbd "ESC C-<down>"))
(global-unset-key (kbd "ESC C-<end>"))
(global-unset-key (kbd "ESC C-<home>"))
(global-unset-key (kbd "ESC C-<left>"))
(global-unset-key (kbd "ESC C-<right>"))
(global-unset-key (kbd "ESC C-<up>"))
(global-unset-key (kbd "C-t"))
(global-unset-key (kbd "M-("))
