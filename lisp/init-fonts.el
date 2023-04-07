;;; init-fonts.el --- Beautifl gui  -*- lexical-binding: t -*-
;;; Commentary: 最佳字体设置（抄袭改变自 centaur emacs）
;;; Code:

(require 'init-const)
(require 'init-funcs)

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



(provide 'init-fonts)
;;; init-fonts.el ends here
