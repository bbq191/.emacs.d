;;; init-misc.el --- Miscellaneous config -*- lexical-binding: t -*-

;; 设定 y/n 替代 yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(use-package which-key)
(which-key-mode)
(which-key-setup-minibuffer)

(provide 'misc)
;; END misc
