;; Functions
(defun move-text-internal (arg)
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

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun get-shell ()
  "Returns a shell as string depending on OS"
  (unless (string-equal system-type "windows-nt")
    ;; Return bash unless on Windows
    "/bin/bash")
  "powershell")

(defun kill-other-buffers ()
  "kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun scroll-down-half ()
  (interactive)
  (scroll-up (window-half-height)))

(defun scroll-up-half ()         
  (interactive)                    
  (scroll-down (window-half-height)))

(defun create-scratch-buffer nil
  "create a scratch buffer"
  ;; xdd
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))   

(defun eval-config ()
  "Evaluate config.el, which hosts my configuration code"
  (interactive)
  (load-file (expand-file-name "config.el" user-emacs-directory)))

(defun get-file-extension-of-current-file ()
  "Gets file extension of arg"
  (substring (buffer-name) -3))

(defun automatic-babel-tangle ()
;; Automatically org-babel-tangles if document ends in .org"
   (if
       (string= "org" (get-file-extension-of-current-file))
       (org-babel-tangle)))

(defun swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)))
