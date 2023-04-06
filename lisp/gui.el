;;; init-gui.el --- Beautifl gui  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-const)
(require 'init-funcs)

;; 加载 doom themes
(use-package doom-themes
  :config
  (load-theme 'doom-one-light t))

;; 加载 doom modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; 最佳字体设置（抄袭自 centaur emacs）
(defun centaur-setup-fonts ()
  "处理安装各类型字体."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("CaskaydiaCove Nerd Font Mono" "DaddyTimeMono Nerd Font Mono")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height (cond (sys/macp 150)
                                                      (t 100))))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Caskaydiacove Nerd Font Mono" "Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (if (< emacs-major-version 27)
                        (set-fontset-font "fontset-default" 'unicode font nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (cond
                     ((< emacs-major-version 27)
                      (set-fontset-font "fontset-default" 'unicode font nil 'prepend))
                     ((< emacs-major-version 28)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
                     (t
                      (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("Source Han Sans SC" "PingFang SC" "Microsoft Yahei" "STFangsong")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.3)))
                      (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family font))))))

(centaur-setup-fonts)
(add-hook 'window-setup-hook #'centaur-setup-fonts)
(add-hook 'server-after-make-frame-hook #'centaur-setup-fonts)



(provide 'gui)
;;; gui.el ends here
