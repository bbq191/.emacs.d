;; init-ivy.el --- Initialize compant configurations. -*- lexical-binding: t -*-

(use-package company
  :hook (after-init . global-company-mode)
  :config (setq company-minimum-prefix-length 1
                company-show-quick-access t))

(provide 'company)
;; END company.el
