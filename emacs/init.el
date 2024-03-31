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
(use-package affe)

;; Vertico
(use-package vertico
  :init
  (vertico-mode))

(use-package ef-themes)
