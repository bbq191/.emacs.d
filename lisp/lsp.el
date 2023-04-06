;;;
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (defun lsp-save-actions ()
    "LSP actions before save."
    (add-hook 'before-save-hook #'lsp-organize-imports t t)
	(add-hook 'before-save-hook #'lsp-format-buffer t t))
  :hook ((lsp-mode . #'lsp-enable-which-key-integration)
         (lsp-mode . #'lsp-save-actions)
         ((rust-mode) . lsp-deferred))
  :config
  (setq lsp-auto-guess-root t
	lsp-headerline-breadcrumb-enable nil
	lsp-keymap-prefix "C-c l"
	lsp-log-io nil)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package lsp-ui
    :after lsp-mode
    :init
    (setq lsp-ui-doc-include-signature t
	      lsp-ui-doc-position 'at-point
          lsp-ui-sideline-ignore-duplicate t)
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)
    (add-hook 'lsp-ui-mode-hook 'lsp-modeline-code-actions-mode)
    :config
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package dap-mode
    :init
    (add-hook 'lsp-mode-hook 'dap-mode)
    (add-hook 'dap-mode-hook 'dap-ui-mode)
    (add-hook 'dap-mode-hook 'dap-tooltip-mode)
    (add-hook 'rust-mode-hook (lambda() (require 'dap-rust))))
    ;;   (add-hook 'go-mode-hook (lambda() (require 'dap-go)))
    ;;   (add-hook 'java-mode-hook (lambda() (require 'dap-java))))


(provide 'lsp)

;;; lsp.el ends here
