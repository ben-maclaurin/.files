;; Package
(require 'package)

(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/")
 t)

(package-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))

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
;; (global-display-line-numbers-mode t)

;; Affe
(use-package affe
  :bind
  (("M-p" . affe-find)
   ("M-P" . affe-grep)))

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
(global-unset-key (kbd "M-C"))

(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "M-m") 'move-beginning-of-line)

;; For installing tree-sitter grammars see: https://www.masteringemacs.org/article/how-to-get-started-tree-sitter

(use-package prettier)
(use-package docker
  :bind
  ("M-D" . docker))

(add-to-list 'exec-path "~/bin")

;; Core packages (can't live without)
(use-package vertico
  :init
  (vertico-mode))

(use-package consult
  :bind 
  ("C-c m" . consult-man)
  ("C-c i" . consult-info)
  ("C-x b" . consult-buffer)               
  ("C-x 4 b" . consult-buffer-other-window)
  ("C-x 5 b" . consult-buffer-other-frame) 
  ("C-x t b" . consult-buffer-other-tab)   
  ("C-x r b" . consult-bookmark)           
  ("M-s r" . consult-ripgrep)
  ("M-s l" . consult-line))
           
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package embark)

(use-package magit)

(use-package helpful)

(set-face-attribute 'variable-pitch nil :family "Berkeley Mono Variable")

(setq-default truncate-lines t)

(setq shell-file-name "/bin/bash")
(setq explicit-shell-file-name "/bin/bash")

(global-set-key (kbd "C-;") 'tailmacs)






