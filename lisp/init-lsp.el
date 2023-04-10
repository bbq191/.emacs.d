;; init-lsp.el --- Initialize LSP configurations.	-*- lexical-binding: t -
;;; Commentary:
;; Language Server Protocol (LSP) configurations.
;;; Code:

(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

(pcase my-lsp
  ('eglot
   (use-package eglot
     :hook ((prog-mode . (lambda ()
                           (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                             (eglot-ensure))))
            ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))))
  ('lsp-mode
   ;; Performace tuning
   ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance/
   (setq read-process-output-max (* 1024 1024)) ;; 1MB
   (setenv "LSP_USE_PLISTS" "true")

   ;; Emacs client for the Language Server Protocol
   ;; https://github.com/emacs-lsp/lsp-mode#supported-languages
   (use-package lsp-mode
     :diminish
     :defines (lsp-diagnostics-disabled-modes lsp-clients-python-library-directories)
     :autoload lsp-enable-which-key-integration
     :commands (lsp-format-buffer lsp-organize-imports)
     :hook ((prog-mode . (lambda ()
                           (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                             (lsp-deferred))))
            ((markdown-mode yaml-mode yaml-ts-mode) . lsp-deferred)
            (lsp-mode . (lambda ()
                          ;; Integrate `which-key'
                          (lsp-enable-which-key-integration)

                          ;; Format and organize imports
                          (when (and lsp-format-on-save
                                     (not (apply #'derived-mode-p lsp-format-on-save-ignore-modes)))
                            (add-hook 'before-save-hook #'lsp-format-buffer t t)
                            (add-hook 'before-save-hook #'lsp-organize-imports t t)))))
     :bind (:map lsp-mode-map
                 ("C-c C-d" . lsp-describe-thing-at-point)
                 ([remap xref-find-definitions] . lsp-find-definition)
                 ([remap xref-find-references] . lsp-find-references))
     :init (setq lsp-keymap-prefix "C-c l"
                 lsp-keep-workspace-alive nil
                 lsp-signature-auto-activate nil
                 lsp-modeline-code-actions-enable nil
                 lsp-modeline-diagnostics-enable nil
                 lsp-modeline-workspace-status-enable nil
                 lsp-headerline-breadcrumb-enable nil

                 lsp-semantic-tokens-enable t
                 lsp-progress-spinner-type 'progress-bar-filled

                 lsp-enable-file-watchers nil
                 lsp-enable-folding nil
                 lsp-enable-symbol-highlighting nil
                 lsp-enable-text-document-color nil

                 lsp-enable-indentation nil
                 lsp-enable-on-type-formatting nil

                 ;; For diagnostics
                 lsp-diagnostics-disabled-modes '(markdown-mode gfm-mode)

                 ;; For clients
                 lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
     :config
     (with-no-warnings
       ;; Disable `lsp-mode' in `git-timemachine-mode'
       (defun my-lsp--init-if-visible (fn &rest args)
         (unless (bound-and-true-p git-timemachine-mode)
           (apply fn args)))
       (advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible)

       ;; Enable `lsp-mode' in sh/bash/zsh
       (defun my-lsp-bash-check-sh-shell (&rest _)
         (and (memq major-mode '(sh-mode bash-ts-mode))
              (memq sh-shell '(sh bash zsh))))
       (advice-add #'lsp-bash-check-sh-shell :override #'my-lsp-bash-check-sh-shell)
       (add-to-list 'lsp-language-id-configuration '(bash-ts-mode . "shellscript"))

       ;; Only display icons in GUI
       (defun my-lsp-icons-get-symbol-kind (fn &rest args)
         (and (icon-displayable-p) (apply fn args)))
       (advice-add #'lsp-icons-get-by-symbol-kind :around #'my-lsp-icons-get-symbol-kind)

       (defun my-lsp-icons-get-by-file-ext (fn &rest args)
         (and (icon-displayable-p) (apply fn args)))
       (advice-add #'lsp-icons-get-by-file-ext :around #'my-lsp-icons-get-by-file-ext)

       (defun my-lsp-icons-all-the-icons-material-icon (icon-name face fallback &optional feature)
         (if (and (icon-displayable-p)
                  (lsp-icons--enabled-for-feature feature))
             (all-the-icons-material icon-name
                                     :face face)
           (propertize fallback 'face face)))
       (advice-add #'lsp-icons-all-the-icons-material-icon
                   :override #'my-lsp-icons-all-the-icons-material-icon)))

   (use-package lsp-ui
     :custom-face
     (lsp-ui-sideline-code-action ((t (:inherit warning))))
     :pretty-hydra
     ((:title (pretty-hydra-title "LSP UI" 'faicon "rocket" :face 'all-the-icons-green)
              :color amaranth :quit-key ("q" "C-g"))
      ("Doc"
       (("d e" (progn
                 (lsp-ui-doc-enable (not lsp-ui-doc-mode))
                 (setq lsp-ui-doc-enable (not lsp-ui-doc-enable)))
         "enable" :toggle lsp-ui-doc-mode)
        ("d s" (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature))
         "signature" :toggle lsp-ui-doc-include-signature)
        ("d t" (setq lsp-ui-doc-position 'top)
         "top" :toggle (eq lsp-ui-doc-position 'top))
        ("d b" (setq lsp-ui-doc-position 'bottom)
         "bottom" :toggle (eq lsp-ui-doc-position 'bottom))
        ("d p" (setq lsp-ui-doc-position 'at-point)
         "at point" :toggle (eq lsp-ui-doc-position 'at-point))
        ("d h" (setq lsp-ui-doc-header (not lsp-ui-doc-header))
         "header" :toggle lsp-ui-doc-header)
        ("d f" (setq lsp-ui-doc-alignment 'frame)
         "align frame" :toggle (eq lsp-ui-doc-alignment 'frame))
        ("d w" (setq lsp-ui-doc-alignment 'window)
         "align window" :toggle (eq lsp-ui-doc-alignment 'window)))
       "Sideline"
       (("s e" (progn
                 (lsp-ui-sideline-enable (not lsp-ui-sideline-mode))
                 (setq lsp-ui-sideline-enable (not lsp-ui-sideline-enable)))
         "enable" :toggle lsp-ui-sideline-mode)
        ("s h" (setq lsp-ui-sideline-show-hover (not lsp-ui-sideline-show-hover))
         "hover" :toggle lsp-ui-sideline-show-hover)
        ("s d" (setq lsp-ui-sideline-show-diagnostics (not lsp-ui-sideline-show-diagnostics))
         "diagnostics" :toggle lsp-ui-sideline-show-diagnostics)
        ("s s" (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol))
         "symbol" :toggle lsp-ui-sideline-show-symbol)
        ("s c" (setq lsp-ui-sideline-show-code-actions (not lsp-ui-sideline-show-code-actions))
         "code actions" :toggle lsp-ui-sideline-show-code-actions)
        ("s i" (setq lsp-ui-sideline-ignore-duplicate (not lsp-ui-sideline-ignore-duplicate))
         "ignore duplicate" :toggle lsp-ui-sideline-ignore-duplicate))
       "Action"
       (("h" backward-char "←")
        ("j" next-line "↓")
        ("k" previous-line "↑")
        ("l" forward-char "→")
        ("C-a" mwim-beginning-of-code-or-line nil)
        ("C-e" mwim-end-of-code-or-line nil)
        ("C-b" backward-char nil)
        ("C-n" next-line nil)
        ("C-p" previous-line nil)
        ("C-f" forward-char nil)
        ("M-b" backward-word nil)
        ("M-f" forward-word nil)
        ("c" lsp-ui-sideline-apply-code-actions "apply code actions"))))
     :bind (("C-c u" . lsp-ui-imenu)
            :map lsp-ui-mode-map
            ("M-<f6>" . lsp-ui-hydra/body)
            ("s-<return>" . lsp-ui-sideline-apply-code-actions)
            ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
            ([remap xref-find-references] . lsp-ui-peek-find-references))
     :hook (lsp-mode . lsp-ui-mode)
     :init
     (setq lsp-ui-sideline-show-diagnostics nil
           lsp-ui-sideline-ignore-duplicate t
           lsp-ui-doc-delay 0.1
           lsp-ui-doc-show-with-cursor (not (display-graphic-p))
           lsp-ui-imenu-auto-refresh 'after-save
           lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                 ,(face-foreground 'font-lock-string-face)
                                 ,(face-foreground 'font-lock-constant-face)
                                 ,(face-foreground 'font-lock-variable-name-face)))
     ;; Set correct color to borders
     (defun my-lsp-ui-doc-set-border ()
       "Set the border color of lsp doc."
       (setq lsp-ui-doc-border
             (if (facep 'posframe-border)
                 (face-background 'posframe-border nil t)
               (face-background 'region nil t))))
     (my-lsp-ui-doc-set-border)
     (add-hook 'after-load-theme-hook #'my-lsp-ui-doc-set-border t)
     :config
     (with-no-warnings
       ;; Display peek in child frame if possible
       ;; @see https://github.com/emacs-lsp/lsp-ui/issues/441
       (defvar lsp-ui-peek--buffer nil)
       (defun lsp-ui-peek--peek-display (fn src1 src2)
         (if (childframe-workable-p)
             (-let* ((win-width (frame-width))
                     (lsp-ui-peek-list-width (/ (frame-width) 2))
                     (string (-some--> (-zip-fill "" src1 src2)
                               (--map (lsp-ui-peek--adjust win-width it) it)
                               (-map-indexed 'lsp-ui-peek--make-line it)
                               (-concat it (lsp-ui-peek--make-footer)))))
               (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
               (posframe-show lsp-ui-peek--buffer
                              :string (mapconcat 'identity string "")
                              :min-width (frame-width)
                              :internal-border-color (face-background 'posframe-border nil t)
                              :internal-border-width 1
                              :poshandler #'posframe-poshandler-frame-center))
           (funcall fn src1 src2)))
       (defun lsp-ui-peek--peek-destroy (fn)
         (if (childframe-workable-p)
             (progn
               (when (bufferp lsp-ui-peek--buffer)
                 (posframe-hide lsp-ui-peek--buffer))
               (setq lsp-ui-peek--last-xref nil))
           (funcall fn)))
       (advice-add #'lsp-ui-peek--peek-new :around #'lsp-ui-peek--peek-display)
       (advice-add #'lsp-ui-peek--peek-hide :around #'lsp-ui-peek--peek-destroy)

       ;; Handle docs
       (defun my-lsp-ui-doc--handle-hr-lines nil
         (let (bolp next before after)
           (goto-char 1)
           (while (setq next (next-single-property-change (or next 1) 'markdown-hr))
             (when (get-text-property next 'markdown-hr)
               (goto-char next)
               (setq bolp (bolp)
                     before (char-before))
               (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
               (setq after (char-after (1+ (point))))
               (insert
                (concat
                 (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
                 (propertize "\n" 'face '(:height 0.5))
                 (propertize " "
                             ;; :align-to is added with lsp-ui-doc--fix-hr-props
                             'display '(space :height (1))
                             'lsp-ui-doc--replace-hr t
                             'face `(:background ,(face-foreground 'font-lock-comment-face nil t)))
                 ;; :align-to is added here too
                 (propertize " " 'display '(space :height (1)))
                 (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5)))))))))
       (advice-add #'lsp-ui-doc--handle-hr-lines :override #'my-lsp-ui-doc--handle-hr-lines)))

   ;; Ivy integration
   (use-package lsp-ivy
     :after lsp-mode
     :bind (:map lsp-mode-map
                 ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
                 ("C-s-." . lsp-ivy-global-workspace-symbol))
     :config
     (with-no-warnings
       (when (icon-displayable-p)
         (defconst lsp-ivy-symbol-kind-icons
           `(,(all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.15) ; Unknown - 0
             ,(all-the-icons-faicon "file-o" :height 0.9 :v-adjust -0.02) ; File - 1
             ,(all-the-icons-material "view_module" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-lblue) ; Module - 2
             ,(all-the-icons-material "view_module" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue) ; Namespace - 3
             ,(all-the-icons-octicon "package" :height 0.9 :v-adjust -0.15) ; Package - 4
             ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange) ; Class - 5
             ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.02 :face 'all-the-icons-purple) ; Method - 6
             ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02) ; Property - 7
             ,(all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue) ; Field - 8
             ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.02 :face 'all-the-icons-lpurple) ; Constructor - 9
             ,(all-the-icons-material "storage" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange) ; Enum - 10
             ,(all-the-icons-material "share" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-lblue) ; Interface - 11
             ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.02 :face 'all-the-icons-purple) ; Function - 12
             ,(all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue) ; Variable - 13
             ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.02 :face 'all-the-icons-purple) ; Constant - 14
             ,(all-the-icons-faicon "text-width" :height 0.9 :v-adjust -0.02) ; String - 15
             ,(all-the-icons-material "format_list_numbered" :height 0.95 :v-adjust -0.15) ; Number - 16
             ,(all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue) ; Boolean - 17
             ,(all-the-icons-material "view_array" :height 0.95 :v-adjust -0.15) ; Array - 18
             ,(all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-blue) ; Object - 19
             ,(all-the-icons-faicon "key" :height 0.9 :v-adjust -0.02) ; Key - 20
             ,(all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0) ; Null - 21
             ,(all-the-icons-material "format_align_right" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue) ; EnumMember - 22
             ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange) ; Struct - 23
             ,(all-the-icons-octicon "zap" :height 0.9 :v-adjust 0 :face 'all-the-icons-orange) ; Event - 24
             ,(all-the-icons-material "control_point" :height 0.9 :v-adjust -0.15) ; Operator - 25
             ,(all-the-icons-faicon "arrows" :height 0.9 :v-adjust -0.02) ; TypeParameter - 26
             ))

         (lsp-defun my-lsp-ivy--format-symbol-match
                    ((sym &as &SymbolInformation :kind :location (&Location :uri))
                     project-root)
                    "Convert the match returned by `lsp-mode` into a candidate string."
                    (let* ((sanitized-kind (if (length> lsp-ivy-symbol-kind-icons kind) kind 0))
                           (type (elt lsp-ivy-symbol-kind-icons sanitized-kind))
                           (typestr (if lsp-ivy-show-symbol-kind (format "%s " type) ""))
                           (pathstr (if lsp-ivy-show-symbol-filename
                                        (propertize (format " · %s" (file-relative-name (lsp--uri-to-path uri) project-root))
                                                    'face font-lock-comment-face)
                                      "")))
                      (concat typestr (lsp-render-symbol-information sym ".") pathstr)))
         (advice-add #'lsp-ivy--format-symbol-match :override #'my-lsp-ivy--format-symbol-match))))

   ;; Debug
   (use-package dap-mode
     :defines dap-python-executable
     :functions dap-hydra/nil
     :diminish
     :bind (:map lsp-mode-map
                 ("<f5>" . dap-debug)
                 ("M-<f5>" . dap-hydra))
     :hook ((after-init     . dap-auto-configure-mode)
            (dap-stopped    . (lambda (_) (dap-hydra)))
            (dap-terminated . (lambda (_) (dap-hydra/nil)))

            ((python-mode python-ts-mode)            . (lambda () (require 'dap-python)))
            ((ruby-mode ruby-ts-mode)                . (lambda () (require 'dap-ruby)))
            ((go-mode go-ts-mode)                    . (lambda () (require 'dap-go)))
            ((java-mode java-ts-mode jdee-mode)      . (lambda () (require 'dap-java)))
            ((c-mode c-ts-mode c++-mode c++-ts-mode) . (lambda () (require 'dap-lldb)))
            ((objc-mode swift-mode)                  . (lambda () (require 'dap-lldb)))
            (php-mode                                . (lambda () (require 'dap-php)))
            (elixir-mode                             . (lambda () (require 'dap-elixir)))
            ((js-mode js2-mode js-ts-mode)           . (lambda () (require 'dap-chrome)))
            (powershell-mode                         . (lambda () (require 'dap-pwsh))))
     :init (when (executable-find "python3")
             (setq dap-python-executable "python3")))

   ;; `lsp-mode' and `treemacs' integration
   (use-package lsp-treemacs
     :after lsp-mode
     :bind (:map lsp-mode-map
                 ("C-<f8>" . lsp-treemacs-errors-list)
                 ("M-<f8>" . lsp-treemacs-symbols)
                 ("s-<f8>" . lsp-treemacs-java-deps-list))
     :init (lsp-treemacs-sync-mode 1)
     :config
     (with-eval-after-load 'ace-window
       (when (boundp 'aw-ignored-buffers)
         (push 'lsp-treemacs-symbols-mode aw-ignored-buffers)
         (push 'lsp-treemacs-java-deps-mode aw-ignored-buffers)))

     (with-no-warnings
       (when (icon-displayable-p)
         (treemacs-create-theme "my-colors"
           :extends "doom-colors"
           :config
           (progn
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.1 :face 'all-the-icons-blue))
              :extensions (root))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
              :extensions (boolean-data))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "settings_input_component" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-orange))
              :extensions (class))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "palette" :height 0.95 :v-adjust -0.15))
              :extensions (color-palette))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "square-o" :height 0.95 :v-adjust -0.15))
              :extensions (constant))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "file-text-o" :height 0.95 :v-adjust -0.05))
              :extensions (document))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "storage" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-orange))
              :extensions (enumerator))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "format_align_right" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
              :extensions (enumitem))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "bolt" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-orange))
              :extensions (event))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
              :extensions (field))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "search" :height 0.95 :v-adjust -0.05))
              :extensions (indexer))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "filter_center_focus" :height 0.95 :v-adjust -0.15))
              :extensions (intellisense-keyword))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "share" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
              :extensions (interface))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
              :extensions (localvariable))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple))
              :extensions (method))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "view_module" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
              :extensions (namespace))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "format_list_numbered" :height 0.95 :v-adjust -0.15))
              :extensions (numeric))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "control_point" :height 0.95 :v-adjust -0.2))
              :extensions (operator))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
              :extensions (property))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "format_align_center" :height 0.95 :v-adjust -0.15))
              :extensions (snippet))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "text-width" :height 0.9 :v-adjust -0.05))
              :extensions (string))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange))
              :extensions (structure))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "format_align_center" :height 0.95 :v-adjust -0.15))
              :extensions (template))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
              :extensions (collapsed) :fallback "+")
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
              :extensions (expanded) :fallback "-")
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-binary" :height 0.9  :v-adjust 0.0 :face 'font-lock-doc-face))
              :extensions (classfile))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-blue))
              :extensions (default-folder-opened))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-blue))
              :extensions (default-folder))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-green))
              :extensions (default-root-folder-opened))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-green))
              :extensions (default-root-folder))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-binary" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
              :extensions ("class"))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-zip" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
              :extensions (file-type-jar))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
              :extensions (folder-open))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
              :extensions (folder))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-orange))
              :extensions (folder-type-component-opened))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-orange))
              :extensions (folder-type-component))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-green))
              :extensions (folder-type-library-opened))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-green))
              :extensions (folder-type-library))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-pink))
              :extensions (folder-type-maven-opened))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-pink))
              :extensions (folder-type-maven))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'font-lock-type-face))
              :extensions (folder-type-package-opened))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'font-lock-type-face))
              :extensions (folder-type-package))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "plus" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
              :extensions (icon-create))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "list" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
              :extensions (icon-flat))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "share" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-lblue))
              :extensions (icon-hierarchical))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "link" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
              :extensions (icon-link))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "refresh" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
              :extensions (icon-refresh))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "chain-broken" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
              :extensions (icon-unlink))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-alltheicon "java" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange))
              :extensions (jar))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "book" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-green))
              :extensions (library))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "folder-open" :face 'all-the-icons-lblue))
              :extensions (packagefolder-open))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
              :extensions (packagefolder))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
              :extensions (package))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.1 :face 'all-the-icons-blue))
              :extensions (java-project))))

         (setq lsp-treemacs-theme "my-colors"))))

   ;; Python
   (use-package lsp-pyright
     :preface
     ;; Use yapf to format
     (defun lsp-pyright-format-buffer ()
       (interactive)
       (when (and (executable-find "yapf") buffer-file-name)
         (call-process "yapf" nil nil nil "-i" buffer-file-name)))
     :hook (((python-mode python-ts-mode) . (lambda ()
                                              (require 'lsp-pyright)
                                              (add-hook 'after-save-hook #'lsp-pyright-format-buffer t t))))
     :init (when (executable-find "python3")
             (setq lsp-pyright-python-executable-cmd "python3")))

   ;; C/C++/Objective-C
   (use-package ccls
     :defines projectile-project-root-files-top-down-recurring
     :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls)))
     :config
     (with-eval-after-load 'projectile
       (setq projectile-project-root-files-top-down-recurring
             (append '("compile_commands.json" ".ccls")
                     projectile-project-root-files-top-down-recurring)))
     (with-no-warnings
       ;; FIXME: fail to call ccls.xref
       ;; @see https://github.com/emacs-lsp/emacs-ccls/issues/109
       (cl-defmethod my-lsp-execute-command
         ((_server (eql ccls)) (command (eql ccls.xref)) arguments)
         (when-let ((xrefs (lsp--locations-to-xref-items
                            (lsp--send-execute-command (symbol-name command) arguments))))
           (xref--show-xrefs xrefs nil)))
       (advice-add #'lsp-execute-command :override #'my-lsp-execute-command)))

   ;; Swift
   (use-package lsp-sourcekit)

   ;; Julia
   (use-package lsp-julia
     :hook (julia-mode . (lambda () (require 'lsp-julia))))

   ;; Java
   (use-package lsp-java
     :hook ((java-mode java-ts-mode jdee-mode) . (lambda () (require 'lsp-java))))))

(unless my-lsp
  ;; Enable LSP in org babel
  ;; https://github.com/emacs-lsp/lsp-mode/issues/377
  (cl-defmacro lsp-org-babel-enable (lang)
    "Support LANG in org source code block."
    (cl-check-type lang string)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (setq buffer-file-name (or (->> info caddr (alist-get :file))
                                      "org-src-babel.tmp"))
           (pcase my-lsp
             ('eglot
              (when (fboundp 'eglot-ensure)
                (eglot-ensure)))
             ('lsp-mode
              (when (fboundp 'lsp-deferred)
                ;; Avoid headerline conflicts
                (setq-local lsp-headerline-breadcrumb-enable nil)
                (lsp-deferred)))
             (_
              (user-error "LSP:: invalid `my-lsp' type"))))
         (put ',intern-pre 'function-documentation
              (format "Enable `%s' in the buffer of org source block (%s)."
                      my-lsp (upcase ,lang)))

         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))

  (defconst org-babel-lang-list
    '("go" "python" "ipython" "ruby" "js" "css" "sass" "c" "rust" "java" "cpp" "c++"))
  (add-to-list 'org-babel-lang-list "shell")
  (dolist (lang org-babel-lang-list)
    (eval `(lsp-org-babel-enable ,lang))))

(provide 'init-lsp)

;;; init-lsp.el ends here