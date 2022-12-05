;; Deletes already existing config.el file, which means new changes in config.org will take effect
;; (delete-file (expand-file-name "config.el" user-emacs-directory))
;; 
;; ;; Load config.org file's source code blocks, which contain everything I want emacs to evaluate at startup
;; (org-babel-load-file
;;   (expand-file-name "config.org" user-emacs-directory))

;; Loads config.el which contains actual elisp code
(load (expand-file-name "config.el" user-emacs-directory))
