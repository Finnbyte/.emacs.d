;; Deletes already existing config.el file, which means new changes in config.org will take effect
;; (delete-file (expand-file-name "config.el" user-emacs-directory))
;; 
;; ;; Load config.org file's source code blocks, which contain everything I want emacs to evaluate at startup
;; (org-babel-load-file
;;   (expand-file-name "config.org" user-emacs-directory))

;; Loads config.el which contains actual elisp code
(load (expand-file-name "config.el" user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" default))
 '(display-line-numbers-type 'visual)
 '(global-display-line-numbers-mode t)
 '(make-backup-files nil)
 '(package-selected-packages
   '(dired evil-org company beacon elfeed doom-modeline evil-collection counsel general which-key ivy gruvbox-theme use-package))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style nil nil (uniquify))
 '(warning-suppress-types '((use-package) (use-package) (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
