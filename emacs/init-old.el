;; -----------------------------------------------------------
;; --- Base --------------------------------------------------
;; -----------------------------------------------------------

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
(scroll-bar-mode -1)
(tool-bar-mode -1)

(let ((mono-spaced-font "Geist Mono")
      (proportionately-spaced-font "ABC Otto Variable Unlicensed Trial"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 150)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

(use-package modus-themes)
(setq modus-themes-variable-pitch-ui t)
(load-theme 'modus-operandi)

(use-package ef-themes)
(setq ef-themes-variable-pitch-ui t)
(setq standard-themes-variable-pitch-ui t)

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
  (setq completion-category-overrride nil))

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

;; -------------------------------------------------------------
;; --- Agenda --------------------------------------------------
;; -------------------------------------------------------------

(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c k") 'org-capture)

(defun org-capture-inbox ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "i"))

(define-key global-map (kbd "C-c c") 'org-capture-inbox)

(setq org-agenda-files (list "agenda.org"))

(setq org-capture-templates
      '(("d" "Daily" entry
         (file+headline"agenda.org" "Inbox") ; Replace with your file path
         "** TODO %^{Title}
                    :PROPERTIES:
                    :SKILL: %^{Skill}
                    :END:\n" :empty-lines 1)
        ("i" "Inbox" plain
         (file "agenda.org")
         "%?\n")))

;; -------------------------------------------------------------
;; --- Editing -------------------------------------------------
;; -------------------------------------------------------------

(use-package expand-region
  :ensure t
  :bind
  (("M-n" . er/expand-region)
   ("M-p" . er/contract-region)))

(use-package apheleia
  :ensure t)

;; -------------------------------------------------------------
;; --- Consult -------------------------------------------------
;; -------------------------------------------------------------

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

;; -------------------------------------------------------------
;; --- Emacs ---------------------------------------------------
;; -------------------------------------------------------------

(defun ben-emacs-emacs-init ()
  "Open init file."
  (interactive)
  (find-file "~/.emacs.d/README.org"))

(defun ben-emacs-reload ()
  "Reload config"
  (interactive)
  (org-babel-tangle-file "~/.emacs.d/README.org")
  (load-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-c e i") 'ben-emacs-emacs-init)
(global-set-key (kbd "C-c e r") 'ben-emacs-reload)

(global-set-key (kbd "C-c p") 'previous-buffer)
(global-set-key (kbd "C-c n") 'next-buffer)

(setq dired-dwim-target t)

(global-auto-revert-mode)

(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

(global-set-key (kbd "C-c d w") 'wdired-change-to-wdired-mode)

(use-package keycast
  :ensure t)

(add-to-list 'default-frame-alist '(undecorated-round . t))

(setq make-backup-files nil)

(define-key dired-mode-map (kbd "C-j") 'dired-find-file)

;; -------------------------------------------------------------
;; --- LLM -----------------------------------------------------
;; -------------------------------------------------------------

(use-package gptel
  :ensure t)

(setq gptel-api-key "")

(global-set-key (kbd "C-c l") 'gptel-rewrite)
(global-set-key (kbd "C-c o") 'gptel-send)

;; -------------------------------------------------------------
;; --- macOS ---------------------------------------------------
;; -------------------------------------------------------------

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'none)

(use-package crux
  :ensure t
  :bind
  (("C-c m o" . crux-open-with)))

(defun ben-emacs-macos-icloud-drive ()
  "Open my iCloud Drive in Dired"
  (interactive)
  (dired "~/Library/Mobile Documents/com~apple~CloudDocs/"))

(global-set-key (kbd "C-c m i") 'ben-emacs-macos-icloud-drive)

(use-package exec-path-from-shell
  :ensure t)
(exec-path-from-shell-initialize)

;; -------------------------------------------------------------
;; --- Org -----------------------------------------------------
;; -------------------------------------------------------------

(use-package dslide
  :ensure t)
(use-package org-modern
  :ensure t)

(global-org-modern-mode)

(setq org-directory "~/Documents/org/")

(add-hook 'org-mode-hook 'variable-pitch-mode)

;; -------------------------------------------------------------
;; --- Treesit -------------------------------------------------
;; -------------------------------------------------------------

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; -------------------------------------------------------------
;; --- Version control -----------------------------------------
;; -------------------------------------------------------------

(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)))

;; -------------------------------------------------------------
;; --- Org skills ---------------------------------------------
;; -------------------------------------------------------------

(require 'org)

(defcustom org-skills-max-level 99
  "The customizable maximum level for skills."
  :type 'integer
  :group 'org-skills)

(defun org-skills-exp-for-level (level)
  "Calculate the total EXP required to reach LEVEL."
  (if (<= level 1)
      0
    (floor (* (expt level 3) 0.25))))

(defun org-skills-insert-progress-bar (current-exp next-exp)
  "Generate a progress bar string based on CURRENT-EXP and NEXT-EXP.
The progress bar uses █ for completion and ░ for the remaining portion,
followed by a percentage."
  (let* ((progress (/ (float current-exp) next-exp))
	 (percentage (floor (* progress 100)))
	 (bar-width 10)
	 (completed (floor (* progress bar-width)))
	 (remainder (- bar-width completed)))
    (format "%s%s %d%%" 
	    (make-string completed ?█)
	    (make-string remainder ?░)
	    percentage)))

(defun org-skills-update-skill (skill)
  "Update the skill SKILL by adding 1 EXP and refreshing its progress bar in the task properties."
  (let* ((current-level (string-to-number (or (org-entry-get (point) "LEVEL") "1")))
	 (current-exp (string-to-number (or (org-entry-get (point) "EXP") "0")))
	 (next-exp (string-to-number (or (org-entry-get (point) "NEXT-EXP") 
					 (number-to-string (org-skills-exp-for-level 2)))))
	 (new-exp (+ current-exp 1)))
    (while (and (>= new-exp next-exp)
		(< current-level org-skills-max-level)) ;; Ensure we don't exceed max level
      (setq current-level (1+ current-level))
      (setq new-exp (- new-exp next-exp))
      (setq next-exp (org-skills-exp-for-level current-level)))
    ;; Cap the level at MAX_LEVEL and EXP at 0 if MAX_LEVEL is reached
    (when (>= current-level org-skills-max-level)
      (setq current-level org-skills-max-level)
      (setq new-exp 0)
      (setq next-exp (org-skills-exp-for-level current-level)))
    ;; Update org properties
    (org-entry-put (point) "LEVEL" (format "%d/%d" current-level org-skills-max-level))
    (org-entry-put (point) "EXP" (number-to-string new-exp))
    (org-entry-put (point) "NEXT-EXP" (number-to-string next-exp))
    (let ((progress-bar (org-skills-insert-progress-bar new-exp next-exp)))
      (org-entry-put (point) "PROGRESS-BAR" progress-bar))))

(defun org-skills-complete-task ()
  "Hook to add 1 EXP to the skill when a task is marked as DONE."
  (let ((skill (org-entry-get (point) "SKILL")))
    (when skill
      (org-skills-update-skill skill))))

(add-hook 'org-after-todo-state-change-hook
	  (lambda ()
	    (when (string= org-state "DONE")
	      (org-skills-complete-task))))


;; -------------------------------------------------------------
;; --- Copilot -------------------------------------------------
;; -------------------------------------------------------------

;; You have to VC install Copilot
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;; -------------------------------------------------------------
;; --- Howm ----------------------------------------------------
;; -------------------------------------------------------------

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

;; -------------------------------------------------------------
;; --- Spacious Padding ----------------------------------------
;; -------------------------------------------------------------

(use-package spacious-padding
  :ensure t)
(spacious-padding-mode)

;; -------------------------------------------------------------
;; --- Astro --------------------------------------------------
;; -------------------------------------------------------------

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

(set-frame-parameter (selected-frame) 'alpha '(100 . 100)) ; Adjust the numbers for transparencyy
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))

;;(setq-default line-spacing 5)
