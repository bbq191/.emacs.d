;; init-buffer.el --- Initialize ibuffer configurations.	-*- lexical-binding: t -*-
;;; Commentary: IBuffer configurations.
;;; Code:

(require 'init-const)
(require 'init-funcs)

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :config
  ;; Display icons for buffers
  (use-package all-the-icons-ibuffer
    :hook (ibuffer-mode . all-the-icons-ibuffer-mode)
    :init (setq all-the-icons-ibuffer-icon my-icon))

  (with-eval-after-load 'counsel
    (with-no-warnings
      (defun my-ibuffer-find-file ()
        (interactive)
        (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                   (if (buffer-live-p buf)
                                       (with-current-buffer buf
                                         default-directory)
                                     default-directory))))
          (counsel-find-file default-directory)))
      (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file))))

;; Group ibuffer's list by project
(use-package ibuffer-projectile
  :functions all-the-icons-octicon ibuffer-do-sort-by-alphabetic
  :hook ((ibuffer . (lambda ()
                      (ibuffer-projectile-set-filter-groups)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic)))))
  :config
  (setq ibuffer-projectile-prefix
        (if (icon-displayable-p)
            (concat
             (all-the-icons-octicon "repo"
                                    :face ibuffer-filter-group-name-face
                                    :v-adjust 0.0
                                    :height 1.1)
             " ")
          "Project: ")))

(provide 'init-ibuffer)

;;; init-ibuffer.el ends here
