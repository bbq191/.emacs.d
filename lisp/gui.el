;;

(tool-bar-mode -1)
(scroll-bar-mode -1)

;; 加载 doom themes
(use-package doom-themes)
(load-theme 'doom-one-light t)

;; 加载 doom modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))


(provide 'gui)



;;
