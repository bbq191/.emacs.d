;; init.el --- Load the full configuration -*- lexical-binding: t -*-


;; 开启错误 debug
(setq debug-on-error t)

;; 设定源码加载路径
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; 隔离 emacs 自动生成的代码
(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'elpa)
(require 'gui-frame)



(provide 'init.el)
;; END init.el