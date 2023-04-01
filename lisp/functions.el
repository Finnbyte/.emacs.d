;;; Functions
(defun me/move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          (when (and (eval-when-compile
                       '(and (>= emacs-major-version 24)
                             (>= emacs-minor-version 3)))
                     (< arg 0))
            (forward-line -1)))
        (forward-line -1))
      (move-to-column column t)))))

(defun me/move-text-down (arg)
  "Move region (transient-mark-mode active) or current line arg lines down."
  (interactive "*p")
  (me/move-text-internal arg))

(defun me/move-text-up (arg)
  "Move region (transient-mark-mode active) or current line arg lines up."
  (interactive "*p")
  (me/move-text-internal (- arg)))

(defun me/get-shell ()
  "Returns a shell as string depending on OS"
  (unless (string-equal system-type "windows-nt")
    ;; Return bash unless on Windows
    "/bin/bash")
  "powershell")

(defun me/try-execute-cmd (CMD-STR-UNPARSED)
  ;; Return nil if command resulted in error
  ;; Meant to be used for checking if binary is found
  (let (cmd (split-string CMD-STR-UNPARSED " " t))
      (condition-case nil
      (start-process "" nil (car cmd) "-v") ;
    (error nil))))

(defun me/save-persistent-scratch ()
  "Write the contents of *scratch* to the file name
`persistent-scratch-file-name'."
  (with-current-buffer (get-buffer-create "*scratch*")
    (write-region (point-min) (point-max) (expand-file-name ".persistent-scratch" user-emacs-directory))))

(defun me/load-persistent-scratch ()
  "Load the contents of `persistent-scratch-file-name' into the
  scratch buffer, clearing its contents first."
  (if (file-exists-p (expand-file-name ".persistent-scratch" user-emacs-directory))
      (with-current-buffer (get-buffer "*scratch*")
        (delete-region (point-min) (point-max))
        (insert-file-contents (expand-file-name ".persistent-scratch" user-emacs-directory)))))

(defun me/kill-other-buffers ()
  "kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun me/window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun me/scroll-down-half ()
  (interactive)
  (scroll-up (me/window-half-height)))

(defun me/scroll-up-half ()         
  (interactive)                    
  (scroll-down (me/window-half-height)))

(defun me/create-scratch-buffer nil
  "create a scratch buffer"
  ;; xdd
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))   

(defun me/eval-config ()
  "Evaluate config.el, which hosts my configuration code"
  (interactive)
  (load-file (expand-file-name "config.el" user-emacs-directory)))

(defun me/get-filetype (filename)
  "Gets filetype of filename string"
  (if (stringp filename)
      (last (split-string filename "[\.]" t))
  (error (format "Invalid string: '%s'" filename))))

(defun me/current-filename ()
    "Get name of current file being edited"
    (buffer-name))

(defun me/automatic-babel-tangle ()
;; Automatically org-babel-tangles if document ends in .org"
   (if (string= "org" (me/get-filetype (me/current-filename)))
       (org-babel-tangle)))

(defun me/swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)))
