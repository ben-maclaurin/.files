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
(set-frame-font "Comic Code-14" nil t)

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
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

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

(set-face-attribute 'variable-pitch nil :family "Comic Code")

(setq-default truncate-lines t)

(setq shell-file-name "/bin/bash")
(setq explicit-shell-file-name "/bin/bash")

;;(global-set-key (kbd "C-;") 'tailmacs)

(defun bubble-shift-up ()
  "Shift the region up by one line."
  (interactive)
  (when (> (point) (mark))
    (exchange-point-and-mark))
  (forward-line -1)
  (forward-line 0)
  (exchange-point-and-mark)
  (forward-line -1)
  (end-of-line)
  (activate-mark)
  (exchange-point-and-mark))

(defun bubble-shift-down ()
  "Shift the region down by one line."
  (interactive)
  (when (> (point) (mark))
    (exchange-point-and-mark))
  (forward-line)
  (forward-line 0)
  (exchange-point-and-mark)
  (forward-line)
  (end-of-line)
  (activate-mark)
  (exchange-point-and-mark))



(global-set-key (kbd "M-n") 'bubble-shift-down)
(global-set-key (kbd "M-p") 'bubble-shift-up)

(global-set-key (kbd "C-o") 'exchange-point-and-mark)
















