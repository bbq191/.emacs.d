;; init-const.el --- Define constants.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Define constants.
;;

;;; Code:

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")


(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=28p
  (>= emacs-major-version 28)
  "Emacs is 28 or above.")

(defconst emacs/>=29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above.")

(provide 'init-const)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-const.el ends here
