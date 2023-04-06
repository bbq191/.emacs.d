;;; init-osx-keys.el --- Configure specific to MacOS -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'define)

;; 改变键盘设置
(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))
  ;; TODO 考虑设置右 alt 为 ctl 键

(provide 'macos)

;;; macos.el ends here
