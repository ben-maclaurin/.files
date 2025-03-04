
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;;; Set up the package manager

(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;;; Basic behaviour

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

;;; Tweak the looks of Emacs

;; Those three belong in the early-init.el, but I am putting them here
;; for convenience.  If the early-init.el exists in the same directory
;; as the init.el, then Emacs will read+evaluate it before moving to
;; the init.el.
(menu-bar-mode 1)
(scroll-bar-mode 1)
(tool-bar-mode -1)

(let ((mono-spaced-font "EK Modena Mono")
      (proportionately-spaced-font "EK Modena Mono"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 200)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi-tinted :no-confirm-loading))

;;; Configure the minibuffer and completions

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil))

(use-package savehist
  :ensure nil ; it is built-in
  :hook (after-init . savehist-mode))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;;; The file manager (Dired)

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;; Howm

(use-package howm
  :ensure t
  :init
  (require 'howm)
  ;; Where to store the files?
  (setq howm-directory "~/Documents/Howm")
  (setq howm-home-directory howm-directory)
  ;; What format to use for the files?
  (setq howm-file-name-format "%Y-%m-%d-%H%M%S.org")
  (setq howm-view-title-header "*")
  (setq howm-dtime-format "<%Y-%m-%d %a %H:%M>")
  :bind*
  ("C-;" . howm-menu))

(add-hook ' howm-view-contents-mode-hook 'variable-pitch-mode)

(define-key howm-mode-map (kbd "C-j") 'action-lock-magic-return)

(defun my-modus-themes-custom-faces (&rest _)
       (modus-themes-with-colors
         (custom-set-faces
          `(action-lock-face ((,c :inherit button)))
          `(howm-mode-keyword-face (( )))
          `(howm-mode-ref-face ((,c :inherit link)))
          `(howm-mode-title-face ((,c :inherit modus-themes-heading-0)))
          `(howm-mode-wiki-face ((,c :inherit link)))
          `(howm-reminder-deadline-face ((,c :foreground ,date-deadline)))
          `(howm-reminder-late-deadline-face ((,c :inherit bold :foreground ,date-deadline)))
          `(howm-reminder-defer-face ((,c :foreground ,date-scheduled)))
          `(howm-reminder-scheduled-face ((,c :foreground ,date-scheduled)))
          `(howm-reminder-done-face ((,c :foreground ,prose-done)))
          `(howm-reminder-todo-face ((,c :foreground ,prose-todo)))
          `(howm-reminder-normal-face ((,c :foreground ,date-common)))
          `(howm-reminder-today-face ((,c :inherit bold :foreground ,date-common)))
          `(howm-reminder-tomorrow-face ((,c :inherit bold :foreground ,date-scheduled)))
          `(howm-simulate-todo-mode-line-face ((,c :inherit bold)))
          `(howm-view-empty-face (( )))
          `(howm-view-hilit-face ((,c :inherit match)))
          `(howm-view-name-face ((,c :inherit bold)))
          `(iigrep-counts-face1 ((,c :foreground ,rainbow-1)))
          `(iigrep-counts-face2 ((,c :foreground ,rainbow-2)))
          `(iigrep-counts-face3 ((,c :foreground ,rainbow-3)))
          `(iigrep-counts-face4 ((,c :foreground ,rainbow-4)))
          `(iigrep-counts-face5 ((,c :foreground ,rainbow-5))))))

     (add-hook 'enable-theme-functions #'my-modus-themes-custom-faces) 

;; package-vc-install https://github.com/jdtsmith/ultra-scroll

;; Swift Development

;;; Locate sourcekit-lsp
(defun find-sourcekit-lsp ()
  (or (executable-find "sourcekit-lsp")
      (and (eq system-type 'darwin)
           (string-trim (shell-command-to-string "xcrun -f sourcekit-lsp")))
      "/usr/local/swift/usr/bin/sourcekit-lsp"))

(use-package lsp-mode
    :ensure t
    :commands lsp
    :hook ((swift-mode . lsp)))

;; lsp-mode's UI modules
(use-package lsp-ui
    :ensure t)

;; sourcekit-lsp support
(use-package lsp-sourcekit
    :ensure t
    :after lsp-mode
    :custom
    (lsp-sourcekit-executable (find-sourcekit-lsp) "Find sourcekit-lsp"))


;; Treesit

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

;; Astro

(use-package web-mode
  :ensure t)

(define-derived-mode astro-mode web-mode "astro")
(setq auto-mode-alist
      (append '((".*\\.astro\\'" . astro-mode))
              auto-mode-alist))

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs
               '(astro-mode . ("astro-ls" "--stdio"
                               :initializationOptions
                               (:typescript (:tsdk "./node_modules/typescript/lib")))))
  :init
  (add-hook 'astro-mode-hook 'eglot-ensure))

;; Small macOS fixes

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'none)

(use-package exec-path-from-shell
  :ensure t)
(exec-path-from-shell-initialize)


(use-package consult
  :ensure t
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("C-x C-f" . consult-fd)
   ("C-x C-r" . consult-ripgrep)
   ("M-s l" . consult-line)  
   ("M-s m" . consult-mark)
   ("M-s k" . consult-kmacro)
   ("M-y" . consult-yank-pop)) 
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0
	register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window))

(setq consult-async-min-input 0)
(setq consult-async-input-debounce 0.001)
(setq consult-async-refresh-delay 0.001)
(setq consult-async-input-throttle 0.001)


;; Optional: Disable theme safety for custom themes.
(setq custom-safe-themes t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Now load (enable) the theme.
(load-theme 'usgc-theme t)
