;; init-hydra.el --- Initialize hydra configurations. -*- lexical-binding: t -*-
;;; Commentary:  Nice looking hydras.
;;; Code:

(require 'init-custom)
(require 'init-funcs)

(use-package hydra
  :hook (emacs-lisp-mode . hydra-add-imenu))

(use-package pretty-hydra
  :bind ("<f6>" . toggles-hydra/body)
  :hook (emacs-lisp-mode . (lambda ()
                             (add-to-list
                              'imenu-generic-expression
                              '("Hydras"
                                "^.*(\\(pretty-hydra-define\\) \\([a-zA-Z-]+\\)"
                                2))))
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icon-displayable-p) icon-type icon-name)
         (let ((f (intern (format "all-the-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face))))

  ;; Global toggles
  (with-no-warnings
    (pretty-hydra-define toggles-hydra (:title (pretty-hydra-title "Toggles" 'faicon "toggle-on" :v-adjust -0.1)
                                               :color amaranth :quit-key ("q" "C-g"))
      ("Basic"
       (("n" (cond ((fboundp 'display-line-numbers-mode)
                    (display-line-numbers-mode (if display-line-numbers-mode -1 1)))
                   ((fboundp 'gblobal-linum-mode)
                    (global-linum-mode (if global-linum-mode -1 1))))
         "line number"
         :toggle (or (bound-and-true-p display-line-numbers-mode)
                     (bound-and-true-p global-linum-mode)))
        ("a" global-aggressive-indent-mode "aggressive indent" :toggle t)
        ("d" global-hungry-delete-mode "hungry delete" :toggle t)
        ("e" electric-pair-mode "electric pair" :toggle t)
        ;; ("c" flyspell-mode "spell check" :toggle t)
        ("s" prettify-symbols-mode "pretty symbol" :toggle t)
        ("l" global-page-break-lines-mode "page break lines" :toggle t)
        ("b" display-battery-mode "battery" :toggle t)
        ("i" display-time-mode "time" :toggle t)
        ("m" doom-modeline-mode "modern mode-line" :toggle t))
       "Highlight"
       (("h l" global-hl-line-mode "line" :toggle t)
        ("h p" show-paren-mode "paren" :toggle t)
        ("h s" symbol-overlay-mode "symbol" :toggle t)
        ("h r" rainbow-mode "rainbow" :toggle t)
        ("h w" (setq-default show-trailing-whitespace (not show-trailing-whitespace))
         "whitespace" :toggle show-trailing-whitespace)
        ("h d" rainbow-delimiters-mode "delimiter" :toggle t)
        ("h i" highlight-indent-guides-mode "indent" :toggle t)
        ("h t" global-hl-todo-mode "todo" :toggle t))
       "Program"
       (("f" flycheck-mode "flycheck" :toggle t)
        ("F" flymake-mode "flymake" :toggle t)
        ("O" hs-minor-mode "hideshow" :toggle t)
        ("u" subword-mode "subword" :toggle t)
        ("W" which-function-mode "which function" :toggle t)
        ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
        ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
        ("v" global-diff-hl-mode "gutter" :toggle t)
        ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
        ("M" diff-hl-margin-mode "margin gutter" :toggle t)
        ("D" diff-hl-dired-mode "dired gutter" :toggle t))
       "Theme"
       (("t a" (load-theme 'auto) "auto"
         :toggle (eq my-theme 'auto) :exit t)
        ("t m" (load-theme 'random) "random"
         :toggle (eq my-theme 'random) :exit t)
        ("t s" (load-theme 'system) "system"
         :toggle (eq my-theme 'system) :exit t)
        ("t d" (load-theme 'default) "default"
         :toggle (theme-enable-p 'default) :exit t)
        ("t p" (load-theme 'pro) "pro"
         :toggle (theme-enable-p 'pro) :exit t)
        ("t k" (load-theme 'dark) "dark"
         :toggle (theme-enable-p 'dark) :exit t)
        ("t l" (load-theme 'light) "light"
         :toggle (theme-enable-p 'light) :exit t)
        ("t w" (load-theme 'warm) "warm"
         :toggle (theme-enable-p 'warm) :exit t)
        ("t c" (load-theme 'cold) "cold"
         :toggle (theme-enable-p 'cold) :exit t)
        ("t y" (load-theme 'day) "day"
         :toggle (theme-enable-p 'day) :exit t)
        ("t n" (load-theme 'night) "night"
         :toggle (theme-enable-p 'night) :exit t)
        ("t o" (ivy-read "Load custom theme: "
                         (all-completions "doom" (custom-available-themes))
                         :action (lambda (theme)
                                   (set-variable
                                    'theme
                                    (let ((x (intern theme)))
                                      (or (car (rassoc x theme-alist)) x)))
                                   (counsel-load-theme-action theme))
                         :caller 'counsel-load-theme)
         "others"
         :toggle (not (or (rassoc (car custom-enabled-themes) theme-alist)
                          (rassoc (cadr custom-enabled-themes) theme-alist)))
         :exit t))
       "Package Archive"
       (("p m" (set-package-archives 'melpa t)
         "melpa" :toggle (eq package-archives 'melpa) :exit t)
        ("p c" (set-package-archives 'emacs-cn t)
         "emacs-cn" :toggle (eq package-archives 'emacs-cn) :exit t)
        ("p b" (set-package-archives 'bfsu t)
         "bfsu" :toggle (eq package-archives 'bfsu) :exit t)
        ("p n" (set-package-archives 'netease t)
         "netease" :toggle (eq package-archives 'netease) :exit t)
        ("p s" (set-package-archives 'sjtu t)
         "sjtu" :toggle (eq package-archives 'sjtu) :exit t)
        ("p t" (set-package-archives 'tuna t)
         "tuna" :toggle (eq package-archives 'tuna) :exit t)
        ("p u" (set-package-archives 'ustc t)
         "ustc" :toggle (eq package-archives 'ustc) :exit t)
        ("p T" (test-package-archives) "speed test" :exit t))))))

(provide 'init-hydra)

;;; init-hydra.el ends here
