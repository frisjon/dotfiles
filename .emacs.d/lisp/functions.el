(defun fris/backward-kill-char-or-word ()
  "To replace default backward-kill-word
Taken from https://emacs.stackexchange.com/questions/30401/backward-kill-word-kills-too-much-how-to-make-it-more-intelligent
Bound to C-Backspace. See bindings.el"
  (interactive)
  (cond
   ((looking-back (rx (char word)) 1)
    (backward-kill-word 1))
   ((looking-back (rx (char blank)) 1)
    (delete-horizontal-space t))
   (t
    (backward-delete-char 1))))

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

(defun fris/new-empty-buffer-in-window ()
  ""
  (interactive)
  (edwina-clone-window)
  (fris/xah-new-empty-buffer))

(defun fris/delete-window ()
  (interactive)
  (progn
    (delete-window)
    (rotate-layout)))

(defun fris/find-file-wsl ()
  "find-file in remote location.
Use on windows. Requires to have putty installed, with a remote location saved as local_wsl.
In this case, the remote location is WSL running on windows. WSL must have ssh installed and running."
  (interactive)
  (find-file "/plinkx:local_wsl:~/"))

(defun fris/kill-buffer-and-window ()
  (interactive)
  (kill-buffer-and-window)
  (rotate-layout)
  )
