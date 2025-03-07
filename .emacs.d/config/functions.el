;; -*- lexical-binding: t; -*-
(defun fris/emacs-startup-time ()
  "Profile emacs startup"
  (message "*** Emacs loaded in %s seconds with %d garbage collections."
    (emacs-init-time "%.2f") gcs-done))

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
  "find-file in remote location."
  (interactive)
  (find-file "/plinkx:local_wsl:~/"))

(defun fris/edwina-kill-buffer-and-window ()
  (interactive)
  (kill-buffer-and-window)
  (edwina-arrange)
  (fris/edwina-focus-master))

(defun fris/edwina-zoom ()
  (interactive)
  (edwina-zoom)
  (fris/edwina-focus-master))

(defun fris/edwina-delete-window()
  (interactive)
  (edwina-delete-window)
  (fris/edwina-focus-master))

(defun fris/edwina-open-ibuffer-in-new-window()
  (interactive)
  (edwina-clone-window)
  (fris/edwina-focus-master)
  (ibuffer nil))

(defun fris/color-name-string-to-hex-string (color)
  "Takes in string color-name variable and returns its hex code as a string"
  (apply 'format "#%02x%02x%02x"
    (mapcar (lambda (c) (ash c -8))
      (color-values color))))

(defalias 'fris/color 'fris/color-name-string-to-hex-string)

;; write a function to do the spacing
(defun fris-modeline/simple-mode-line-render (left right)
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

(defun fris/insert-character-with-font (char font)
  "Insert CHAR into the current buffer with FONT.
Taken from https://emacs.stackexchange.com/a/79976"
  (let ((start (point)))
    (insert char)
    (add-text-properties start (point)
                         `(face (:family ,font)))))

(defun fris/print-all-fonts ()
  "Print details of all installed fonts.
Taken from https://emacs.stackexchange.com/a/79976"
  (interactive)
  (font-lock-mode -1)
  (set-fontset-font t 'symbol nil)
  (set-fontset-font t 'emoji nil)
  (let ((fonts (font-family-list)))
    (dolist (font fonts)
      (fris/insert-character-with-font "🔁" font) ;; 01F501 - Segoe UI Symbol is best
      (insert " ")
      (fris/insert-character-with-font "🧗" font) ;; 01F9D7 - only in Segoe UI Emoji
      (insert (prin1-to-string font))
      (insert "\n"))))

(defun fris/highlight-word ()
  "Highlight the current word you are on.
https://www.reddit.com/r/emacs/comments/1bgyq3y/wrote_my_first_function/"
  (interactive)
  (backward-word 1)
  (set-mark-command nil)
  (forward-word 1))

(defun fris/xah-next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (fris/xah-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun fris/xah-previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (fris/xah-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun fris/xah-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil. version 2016-06-18"
  (interactive)
  (if (and (string-equal "*" (substring (buffer-name) 0 1)) (not (string-equal "*scratch*" (buffer-name))) (not (string-equal "*Help*" (buffer-name))))
      nil
    t)
  )

(defun ajv/human-readable-file-sizes-to-bytes (string)
  "Convert a human-readable file size into bytes.
https://www.emacswiki.org/emacs/IbufferMode#h5o-1"
  (interactive)
  (cond
   ((string-suffix-p "G" string t)
    (* 1000000000 (string-to-number (substring string 0 (- (length string) 1)))))
   ((string-suffix-p "M" string t)
    (* 1000000 (string-to-number (substring string 0 (- (length string) 1)))))
   ((string-suffix-p "K" string t)
    (* 1000 (string-to-number (substring string 0 (- (length string) 1)))))
   (t (string-to-number (substring string 0 (- (length string) 1))))))

(defun ajv/bytes-to-human-readable-file-sizes (bytes)
  "Convert number of bytes to human-readable file size.
https://www.emacswiki.org/emacs/IbufferMode#h5o-1"
  (interactive)
  (cond
   ((> bytes 1000000000) (format "%10.1fG" (/ bytes 1000000000.0)))
   ((> bytes 100000000) (format "%10.0fM" (/ bytes 1000000.0)))
   ((> bytes 1000000) (format "%10.1fM" (/ bytes 1000000.0)))
   ((> bytes 100000) (format "%10.0fk" (/ bytes 1000.0)))
   ((> bytes 1000) (format "%10.1fk" (/ bytes 1000.0)))
   (t (format "%10d" bytes))))

(defun fris/remove-box-attr-from-modeline ()
  (interactive)
  (set-face-attribute 'mode-line nil :box nil) ;; remove box attribute from modeline
      (set-face-attribute 'mode-line-inactive nil :box nil))
