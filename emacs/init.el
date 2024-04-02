;; Package
(require 'package)

(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/")
 t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)) ; enable use of use-package
(setq use-package-always-ensure t)

;; Hide custom nonsense
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Font
(set-frame-font "Berkeley Mono Variable-14" nil t)

;; Darwin
(setq mac-command-modifier 'meta)

;; No title bar
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; No scrollbar
(scroll-bar-mode -1)

;; Convenience
(setq next-line-add-newlines t)
(delete-selection-mode 1)

;; No backups, no auto-save
(setq make-backup-files nil)
(setq auto-save-default nil)
(global-auto-revert-mode 1) ; listen for local file change

;; Smooth scrolling
(pixel-scroll-precision-mode 1)

;; Line numbers
(global-display-line-numbers-mode t)

;; Magit
(use-package magit)

;; Affe
(use-package affe
  :bind
  (("M-p" . affe-find)
   ("M-P" . affe-grep)))

;; Vertico
(use-package vertico
  :init
  (vertico-mode))

;; Ef-themes
(use-package ef-themes)

;; Avy
(use-package avy
  :bind
  (("C-j" . avy-goto-char-timer)))

;; Expand-region
(use-package
 expand-region
 :init (require 'expand-region)
 :bind
 (("C-l" . er/expand-region)
  ("C-;" . er/contract-region)))

(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "C-c u") 'winner-undo)
(global-set-key (kbd "C-c r") 'winner-redo)

(global-set-key (kbd "C-c p") 'previous-buffer)
(global-set-key (kbd "C-c n") 'next-buffer)

(winner-mode 1)
(global-tab-line-mode 1)

(global-unset-key (kbd "C-x o"))

(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "M-m") 'move-beginning-of-line)

(use-package dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)


