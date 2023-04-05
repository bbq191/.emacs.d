;; elpa.el --- Settings and helpers for package.el -*- lexical-binding: t -*-

;; 需要放到最前面
(require 'package)

;; 防止字节码不兼容，分版本安装到单独目录
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))

;; `t` 参数的作用是将这个元素添加到列表的最前面，这样就可以优先使用 MELPA 软件源来安装和更新包
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)

;; Work-around for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(when (and (version< emacs-version "26.3") (boundp 'libgnutls-version) (>= libgnutls-version 30604))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; 初始化软件包管理器
(unless (bound-and-true-p package--initialized)
    (package-initialize))

;; 刷新软件源索引
(unless package-archive-contents
    (package-refresh-contents))

;; 插件 use-package 用来批量统一管理软件包
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

;; use-package 全局配置管理
(setq use-package-always-ensure t         ;; 确保正确安装
      use-package-always-defer t          ;; 开启延时加载
      use-package-enable-imenu-support t  ;; 开启安装菜单栏
      use-package-verbose t               ;; 开启verbose安装
      use-package-daemon nil              ;; 关闭静默
      use-package-expand-minimally t)     ;; 最小化扩展

(require 'use-package)

(provide 'elpa)
;; END elpa.el
