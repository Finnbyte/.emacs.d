;;; init.el --- Emacs config

;; Author: Finnbyte <https://github.com/finnbyte>
;; Maintainer: Finnbyte <https://github.com/finnbyte>

;;; Commentary:

;; Finnbyte's init.el aka Emacs dotfiles.

;;; Code:

;; Load all elisp source code blocks from my literate config
(org-babel-load-file
 (expand-file-name
  "README.org"
  user-emacs-directory))

(provide 'init)
;;; init.el ends here
