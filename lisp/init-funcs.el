;; init-funcs.el --- Define functions.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Define functions.
;;

;;; Code:

;; Font
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(provide 'init-funcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-funcs.el ends here