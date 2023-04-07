;; init-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t -*-


;; 显示行号
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (setq display-line-numbers-type 'relative)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))


(provide 'editor)

;; END editor.el
