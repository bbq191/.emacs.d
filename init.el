;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;
;; 学习 CENTAUR EMACS 源码
;;
;;; Code:

(when (version< emacs-version "29.1")
  (error "This requires Emacs 29.1 and above!"))

;; 开启错误 debug
(setq debug-on-error t)


;; Prevent flashing of unstyled modeline at startup
(setq-default mode-line-format nil)

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Prevent flashing of messages at startup
  (when (display-graphic-p)
    (setq-default inhibit-redisplay t
                  inhibit-message t)
    (defun reset-inhibit-vars ()
      (setq-default inhibit-redisplay nil
                    inhibit-message nil)
      (redraw-frame))
    (add-hook 'window-setup-hook #'reset-inhibit-vars)
    (define-advice startup--load-user-init-file (:after (&rest _) reset-inhibit-vars)
      (and init-file-had-error (reset-inhibit-vars))))

  ;; Suppress file handlers operations at startup
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'.

Don't put large files in `site-lisp' directory, e.g. EAF.
Otherwise the startup will be very slow. "
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; 隔离 emacs 自动生成的代码
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Packages
;; Without this comment Emacs25 adds (package-initialize) here
;; (require 'init-package)

(require 'elpa)
;;(require 'misc)
(require 'gui)
;;(require 'macos)
(require 'editor)
;;(require 'swiper)
;;(require 'company)
;;(require 'program)
;;(require 'lsp)

(provide 'init.el)
;;; init.el ends here
