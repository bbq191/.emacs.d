;; init-projectile.el --- Initialize projectile configurations.	-*- lexical-binding: t -*-
;;; Commentary: Projectile configurations.
;;; Code:

;; Manage and navigate projects
(use-package projectile
  :diminish
  :bind (:map projectile-mode-map
         ("s-t"   . projectile-find-file) ; `cmd-t' or `super-t'
         ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
        projectile-sort-order 'recentf
        projectile-use-git-grep t)
  :config
  ;; Use the faster searcher to handle project files
  (when (and (not (executable-find "fd"))
             (executable-find "rg"))
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd))))

  ;; Support Perforce project
  (let ((val (or (getenv "P4CONFIG") ".p4config")))
    (add-to-list 'projectile-project-root-files-bottom-up val)))

(provide 'init-projectile)

;;; init-projectile.el ends here
