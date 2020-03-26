;; init.el --- Emacs configuration file -*- lexical-binding: t -*-

;; Optimization
;;; Garbage collection & File name handler
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1
          file-name-handler-alist default-file-name-handler-alist)))

;;; Inhibit resize frame
(setq frame-inhibit-implied-resize t)
