;; init.el --- Load the full configuration -*- lexical-binding: t -*-


;; 开启错误 debug
(setq debug-on-error t)

;; 设定源码加载路径
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; 操作系统判断
(defconst *is-a-mac* (eq system-type 'darwin))

;; 隔离 emacs 自动生成的代码
(setq custom-file (locate-user-emacs-file "custom.el"))


;; startup

(require 'elpa)
(require 'gui)
(require 'macos)

;; 如果 custom 文件已经存在就加载他
(when (file-exists-p custom-file)
  (load-file custome-file))

(provide 'init.el)
;; END init.el
