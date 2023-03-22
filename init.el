;;; init.el --- Emacs config

;; Author: Finnbyte <https://github.com/finnbyte>
;; Maintainer: Finnbyte <https://github.com/finnbyte>

;;; Commentary:
;; Finnbyte's init.el

;;; Code:

;; Don't use outdated bytecode
(setq load-prefer-newer t)

;; Required for package installing etc.
(require 'package)

;; Repositories
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install use-package if it isn't installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure use-package
(require 'use-package)
(setq use-package-always-ensure 't)

;; Load everything on "lisp"-directory
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; important, enable async byte compiling
(use-package async
  :config (async-bytecomp-package-mode 1))

;; use-packages macros..

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
   '("d" . scroll-down-half)
   '("u" . scroll-up-half)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("." . ido-find-file))
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
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
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
;; Unicode bullets instead of stars on headings
(use-package org-bullets
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(use-package dracula-theme)
(use-package gruvbox-theme)
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italics t))

(load-theme 'doom-tokyo-night t)

;; Throw everything custom does to another file
(setq custom-file (expand-file-name ".custom-settings.el" user-emacs-directory))

(setq inhibit-startup-screen t) ;; No vanilla startup-screen
(recentf-mode 1)
(setq initial-buffer-choice #'recentf-open-files) ;; Open a list of recent worked files

(setq truncate-lines t)

;; Setting font
(set-frame-font "JetBrains Mono 12")

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

;; Always gives focus to help windows
(setq help-window-select t)

(setq warning-minimum-level :error)

;; Answer with y/n to yes/no prompts
(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default apropos-do-all t)

(use-package try)

(use-package electric
  :config
  (electric-pair-mode 1))

(use-package wrap-region
  :config (wrap-region-mode))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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
  (lsp-headerline-breadcrumb-mode -1)
  :hook (prog-mode . #'lsp-deferred))
(use-package lsp-ui
  :custom
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics t))

;; Dired tweaks
;;(use-package dired
;; :custom
;; (dired-kill-when-opening-new-dired-buffer t))

;; Fuzzy finding files
(use-package projectile
  :config
  (projectile-mode 1)
  :custom
  (projectile-completion-system 'ivy))

;; Better linear undo/redo

;; Make text navigating a lot easier
(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

;; Show indentations
(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  :config
  (highlight-indent-guides-mode 1))

;; Sorting M-x results
(use-package flx)

;; Ivy does it all.
(use-package ivy
  :custom
  ;; Setting ivy to be fuzzy
  (ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  :config
  (ivy-mode))

;; Autocompletion stuff
(use-package counsel)
(use-package company
  :custom
  (company-minimum-prefix-length 1)
  :hook (after-init . global-company-mode)
  :config (company-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Shows complete keybinds while typing
(use-package which-key
  :custom
  (which-key-popup-type 'minibuffer)
  :config
  (which-key-mode))

;; With one keybinding, spawn a temporary shell
(use-package shell-pop
  :custom
  (shell-pop-term-shell "powershell"))


;; REPL for common lisp (((Going to test SLIME at some point too.)))
(use-package sly)

;; git client
(use-package magit
  :pin melpa)

;; News reader
(use-package elfeed
  :custom
  ;; Cleaning up $HOME since automatically saves there
  (elfeed-db-directory "~/.emacs.d/elfeed")
  :config
  ;; Set feeds to monitor
  (setq elfeed-feeds
        '("https://www.is.fi/rss/tuoreimmat.xml"
          "https://reddit.com/r/linux.rss")))

;; Getting forecasts in Emacs!
(use-package wttrin
  :config
  (setq wttrin-default-cities '("Turku")))

(use-package all-the-icons
  :if (display-graphic-p))

;; Integration with Discord (because flexing Emacs is fun!)
(use-package elcord
  :init
  (setq elcord--editor-name "I AM SUPERIOR TO YOU HAHAHA")
  :config
  (elcord-mode)
  :custom
  (elcord-idle-message "Doing something else than coding... lame."))

(use-package typescript-mode)

(use-package flycheck
  :hook (after-init . global-flycheck-mode))

(use-package go-mode)

(use-package lua-mode)

(use-package js2-mode)


(provide 'init)
;;; init.el ends here
