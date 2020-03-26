;; init.el --- Emacs configuration file -*- lexical-binding: t -*-

;; Optimization
;; built-in emacs package manager
(setq package-enable-at-startup nil)
;(advice-add #'package--ensure-init-file :override #'ignore)

;; ;;; Garbage collection & File name handler
;; (defvar default-file-name-handler-alist file-name-handler-alist)
;; (setq gc-cons-threshold most-positive-fixnum
;;       gc-cons-percentage 0.6
;;       file-name-handler-alist nil)

;; (add-hook 'emacs-startup-hook
;;   (lambda ()
;;     (setq gc-cons-threshold 16777216
;;           gc-cons-percentage 0.1
;;           file-name-handler-alist default-file-name-handler-alist)))

;;; Inhibit resize frame
(setq frame-inhibit-implied-resize t)

;; Package management
;;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t
      use-package-always-defer t
      )

;;; Local package
(defmacro use-feature (name &rest args)
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

;;; org-mode
(use-feature org
  :defer 5)

;; Setting
;;; Frame title
(setq frame-title-format "%b [%m]")

;;; Startup screen
(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)
(fset 'display-startup-echo-area-message #'ignore)

;;; Graphical elements 
;;(menu-bar-mode -1)
(when (display-graphic-p)
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (tool-bar-mode -1))

;;; Dialog box
(setq use-file-dialog nil
      use-dialog-box nil)

;;; Background color
(set-background-color "AntiqueWhite")

;;; Font


;;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4)

;;; Directory
(defconst history-dir (concat user-emacs-directory "history/"))
(defconst etc-dir (concat user-emacs-directory "etc/"))
(defconst lisp-dir (concat user-emacs-directory "lisp/"))

;;; Line and column number
(column-number-mode 1)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(defun linum-cycle ()
  (interactive)
  (cond ((not display-line-numbers)
         (setq display-line-numbers t))
        ((equal display-line-numbers t)
         (setq display-line-numbers 'relative))
        ((equal display-line-numbers 'relative)
         (setq display-line-numbers nil))))

(global-set-key (kbd "C-c C-l") #'linum-cycle)


;; Built-in packages
;;; Backup files
(use-feature files
  :init
  (setq auto-save-list-file-name (concat history-dir "autosave")
        backup-directory-alist '(("." . ,(concat history-dir "backup/")))
        make-backup-files nil
        backup-by-copying t
        create-lockfiles nil
        auto-save-default nil))

;;; Selection
(use-feature delsel
  :init
  (delete-selection-mode +1))

(use-feature simple
  :init
  (setq shift-select-mode nil))

;;; Highlight line
(use-feature hl-line
  :hook
  (prog-mode . hl-line-mode))

;;; Custom edit
(use-feature cus-edit
  :init
  (setq custom-file (concat etc-dir "custom.el")))

;;; Advanced command
(use-feature novice
  :init
  (setq disabled-command-function nil))

;;; Clipboard


;;; History
(use-feature recentf
  :init
  (setq recentf-save-file (concat history-dir "recentf")
        recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1))

(use-feature savehist
  :init
  (setq savehist-file (concat history-dir "savehist")
        savehist-save-minibuffer-history t
        savehist-autosave-interval nil
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-mode 1))

(use-feature saveplace
  :init
  (setq save-place-file (concat history-dir "saveplace"))
  (save-place-mode 1))

(use-feature desktop
  :init
  (setq desktop-dirname (concat etc-dir "desktop")
        desktop-base-file-name "autosave"
        desktop-base-lock-name "autosave-lock"))

;;; Icomplete
(use-feature icomplete
  :init
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-in-buffer t)
  :config
  (icomplete-mode 1))

;;; Dired
(use-feature dired
  :config
  (setq dired-auto-revert-buffer t
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-listing-switches "-Alh"
        delete-by-moving-to-trash t
        dired-dwim-target t)
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode)))

;;; Ibuffer
(use-feature ibuffer
  :defer 3
  :config
  (setq ibuffer-expert t
        ibuffer-use-other-window nil
        ibuffer-show-empty-filter-groups nil
        ibuffer-saved-filter-groups
        '(("Main"
           ("Directories" (mode . dired-mode))
           ("Org" (mode . org-mode))
           ("Programming" (mode . prog-mode))
           ("Markdown" (mode . markdown-mode))
           ("Magit" (or
                    (mode . magit-blame-mode)
                    (mode . magit-cherry-mode)
                    (mode . magit-diff-mode)
                    (mode . magit-log-mode)
                    (mode . magit-process-mode)
                    (mode . magit-status-mode)))
           ("Emacs" (or
                    (name . "^\\*Help\\*$")
                    (name . "^\\*Custom.*")
                    (name . "^\\*Org Agenda\\*$")
                    (name . "^\\*info\\*$")
                    (name . "^\\*scratch\\*$")
                    (name . "^\\*Backtrace\\*$")
                    (name . "^\\*Completions\\*$")
                    (name . "^\\*straight-process\\*$")
                    (name . "^\\*Messages\\*$"))))))
  :hook
  (ibuffer-mode . hl-line-mode)
  (ibuffer-mode . (lambda ()
                    (ibuffer-switch-to-saved-filter-groups "Main")))
  :bind
  (([remap list-buffers] . #'ibuffer)))

;;; Hippie expand
(use-feature hippie-exp
  :defer 0.5
  :config
  (setq hippie-expand-try-functions-list
        '(
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          ;; try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-complete-file-name-partially
          try-complete-file-name
          ;; try-expand-all-abbrevs
          ;; try-expand-list
          ;; try-expand-line
          ))
  :bind
  ("M-/" . hippie-expand))

;; External packages
;;; Ledger
(use-package ledger-mode
  :defer 3)
