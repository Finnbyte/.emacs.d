;;; init.el --- Emacs config

;; Author: Finnbyte <https://github.com/finnbyte>
;; Maintainer: Finnbyte <https://github.com/finnbyte>

;;; Commentary:

;; Finnbyte's init.el aka Emacs dotfiles.
;; This one file sets everything up, even though there is naturally other elisp files.

;;; Code:

;; Variables
(setq home-user-name "bytz") ;; Some packages should only load when I am at home

;; Don't use outdated bytecode
(setq load-prefer-newer t)

;; Performance/startup time increase?
;; Most popular Emacs distributions use this so hopefully it's good
(setq gc-cons-threshold 100000000)

;; Required for package installing etc.
(require 'package)

;; Repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Install use-package if it isn't installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure use-package
(require 'use-package)
(setq use-package-always-ensure 't)

;; Load everything on "lisp"-directory (doesn't work so temporary solution...)
;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(load-file (expand-file-name "lisp/functions.el" user-emacs-directory))

;; Preserve scratch buffer
(me/load-persistent-scratch)
(push #'me/save-persistent-scratch kill-emacs-hook)

;; Start a timer loop which saves scratch buffer every 2 min
(if (not (boundp 'save-persistent-scratch-timer)) 
    (setq save-persistent-scratch-timer
          (run-with-idle-timer 120 t 'me/save-persistent-scratch)))

;; indentation
(setq-default indent-tabs-mode nil
              tab-stop-list ()
              tab-width  4)
(use-package dtrt-indent
  :config (dtrt-indent-global-mode 1))

;; Meow makes my Emacs wonderfully modal!
(use-package meow
  :config
  (defun meow-setup ()
    "Set cheatsheet layout."
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet)
     '("." . ido-find-file)
     '("<SPC>" . counsel-ibuffer))
    (meow-normal-define-key
     '("M-k" . move-text-up)
     '("M-j" . move-text-down)
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . undo-tree-undo)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  (meow-setup)
  (meow-global-mode 1))

;; Keybinds
(global-unset-key (kbd "C-z")) ;; Extremely annoying to accidentally press this instead of C-x
(global-unset-key (kbd "C-r")) ;; Obsolete as C-s uses swiper

;; undo
(use-package undo-tree
  :bind ("C-r" . undo-tree-redo)
  :config (global-undo-tree-mode))

(use-package org
  :custom
  (org-startup-indented t)
  (org-startup-folded t)
  ;; Disable pesky confirm on elisp evaluation
  (org-confirm-babel-evaluate nil)
  ;; Source block indentation is wack by default
  ;; (org-src-preserve-indentation nil)
  ;; (org-src-tab-acts-natively t)
  ;; (org-src-strip-leading-and-trailing-blank-lines t)
  (org-edit-src-content-indentation 0)
  :config
  ;; org-babel languages
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t)
                                                           (C . t)
                                                           (shell . t))))

(use-package gruvbox-theme)
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italics t))

(load-theme 'gruvbox-dark-soft t)

;; Throw everything custom does to another file
(setq custom-file (expand-file-name ".custom-settings.el" user-emacs-directory))

;; No vanilla startup-screen
(setq inhibit-startup-screen t)

;; Saves recent files in cache
(recentf-mode 1)

;; Wrap long lines
(setq truncate-lines t)

;; Setting font
(set-frame-font "JetBrains Mono 13")

;; Line numbers
(setq-default display-line-numbers-type 'visual)
(global-display-line-numbers-mode t)

;; Disabling unimportant GUI stuff
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; Give some breathing room to edges
(set-fringe-mode 10)

;; Always show new lines below cursor
;; (setq scroll-margin 7)

;; Hightlight entire line cursor is on
(global-hl-line-mode)

;; No backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Don't make new buffers on entering directories
(setq dired-kill-when-opening-new-dired-buffer t)

;; Always gives focus to help windows
(setq help-window-select t)

;; Answer with y/n to yes/no prompts
(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default apropos-do-all t)

(use-package try)

(use-package savehist
  :custom
  (savehist-additional-variables '(compile-command))
  :config
  (savehist-mode 1))

(use-package electric
  :config
  (electric-pair-mode 1))

(use-package wrap-region
  :config (wrap-region-mode))

;; (general-define-key
;;  :states '(normal visual)
;;  :keymaps 'override
;;  :prefix "SPC"
;;  ;; Categories
;;  "h" '(help-command :which-key "Help")
;;  "b" '(:ignore t :which-key "Buffers")
;;  "b" '(:ignore t :which-key "Buffers")
;;  "g" '(:ignore t :which-key "Games")
;;  "a" '(:ignore t :which-key "Apps")
;;  "s" '(:ignore t :which-key "Shell")
;;  "f" '(:ignore t :which-key "Files")
;;  "e" '(:ignore t :which-key "Emacs/Elisp")
;;  "o" '(:ignore t :which-key "Org")
;;
;; ;; sub-categories
;; "a m" '(:ignore t :which-key "Math")
;; "o s" '(:ignore t :which-key "src")
;;
;; "b s" '(counsel-switch-buffer :which-key "Switch buffers")
;; "b b" '(ibuffer :which-key "Show buffer list")
;;
;; "b k" '(kill-this-buffer :which-key "Kill current buffer")
;;
;; "b n" '(switch-to-next-buffer :which-key "Switch to next buffer")
;; "b p" '(switch-to-prev-buffer :which-key "Switch to previous buffer")
;;
;; ;; Alternative command for switching (scrolling through) buffers quickly
;; ">" '(switch-to-next-buffer :which-key t)
;; "<" '(switch-to-prev-buffer :which-key t)
;;
;; "a m c" '(calculator :which-key "Simple calculator")
;; "a m m" '(calc :which-key "Advanced calculator")
;;
;; "g s" '(steam-launch :which-key "Launch a game on Steam")
;;
;; "s p" '(shell-pop :which-key "Pop up a shell")
;; "s v" '(vterm :which-key "vterm")
;; "!" '(shell-command :which-key "Shell command")
;; "\"" '(async-shell-command :which-key "Asynchronous shell command")
;;
;; "." '(ido-find-file :which-key "Find file")
;; "/" '(ido-dired :which-key "Find directory")
;; "f r" '(counsel-recentf :which-key "Recent files")
;; "f b" '(bookmark-jump :which-key "Browse bookmarks")
;; "f s" '(save-buffer :which-key "Save current buffer")
;;
;; ;; Alternative command for bookmarks
;; "DEL" '(bookmark-jump :which-key "Browse bookmarks")
;;
;; "RET" '(eval-defun :which-key "Evaluate expression")
;; "e b" '(eval-buffer :which-key "Evaluate entire buffer")
;; "e r" '(eval-region :which-key "Evaluate an region specified with visual-mode")
;; "e l" '(eval-expression :which-key "Evaluate an elisp expression")
;; "e e" '(eval-config :which-key "Reload config.el")
;;
;; "o i" '(org-insert-structure-template :which-key "Insert org structure template")
;; "o e" '(org-export-dispatch :which-key "Exports org-document to other fileformat")
;; "o s e" '(org-edit-src-code :which-key "Edit src-code block")
;;
;; "p" '(counsel-M-x :which-key "M-x"))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))
(use-package tree-sitter-langs)

(use-package mood-line
  :config
  (mood-line-mode 1))

(use-package undo-fu)

(use-package lsp-mode
  :config
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :custom
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics t)
  :hook (prog-mode . (lambda () (interactive) lsp-ui-mode)))


(use-package yasnippet
  :after (yasnippet-snippets)
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets)

;; Fuzzy finding files
(use-package projectile
  :config
  (projectile-mode 1)
  :custom
  (projectile-completion-system 'ivy))

;; Make text navigating a lot easier
(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

;; Ivy does it all.
(use-package ivy
  :custom
  ;; When line empty and backspace is pressed, don't leave minibuffer
  (ivy-on-del-error-function 'ignore)
  :bind (("C-s" . swiper))
  :config
  (use-package flx) ;; Great sorting algoritm
  ;; This didn't work on :custom for some reason
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-fuzzy)
          (t      . ivy--regex-fuzzy)))
  (ivy-mode))

;; Better help
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))

;; Autocompletion stuff
(use-package counsel)
(use-package company
  :custom
  (company-minimum-prefix-length 1)
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  ;; Company integration with yasnippet
  (company-backends '((company-capf :with company-yasnippet)))
  :hook (after-init . global-company-mode)
  :config
  (company-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Shows complete keybinds while typing
(use-package which-key
  :custom
  (which-key-popup-type 'minibuffer)
  :config
  (which-key-mode))

;; Live markdown/org preview
(use-package grip-mode
  :init
  ;; Run pip install if grip python package not found
  (lambda()
    (if (not (string-match "grip" (shell-command-to-string "pip list --disable-pip-version-check")))
	    (start-process "grip-install" nil "pip" "install" "grip")))
  :hook (markdown-mode . grip-mode))

;; REPL for CL (Common Lisp)
(use-package sly
  :if (executable-find "sbcl")
  :custom
  (sly-complete-symbol-function 'sly-simple-completions)
  :bind (:map sly-mode-map ("M-h" . sly-documentation-lookup)))

;; git client
(use-package magit
  :pin melpa
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

;; News reader
(use-package elfeed
  :if (string= home-user-name (user-login-name))
  :custom
  ;; Cleaning up $HOME since automatically saves there
  (elfeed-db-directory "~/.emacs.d/elfeed")
  :config
  ;; Set feeds to monitor
  (setq elfeed-feeds
        '("https://www.is.fi/rss/tuoreimmat.xml"
          "https://reddit.com/r/linux.rss")))

;; Integration with Discord (because flexing Emacs is fun!)
(use-package elcord
  :if (string= home-user-name (user-login-name))
  :config
  (elcord-mode)
  :custom
  (elcord-idle-message "Doing something else than coding... lame."))

;; Programming modes

;; Typescript
(use-package typescript-mode
  :if (executable-find "ts-node"))
(use-package tide
  :if (executable-find "ts-node")
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package flycheck
  :hook (after-init . global-flycheck-mode))

(use-package go-mode
  :if (executable-find "go"))
(use-package lua-mode
  :if (executable-find "lua"))
(use-package js2-mode
  :if (executable-find "node"))

(provide 'init)
;;; init.el ends here
