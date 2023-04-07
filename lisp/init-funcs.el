;; init-funcs.el --- Define functions.	-*- lexical-binding: t -*-
;;; Commentary: Define functions.
;;; Code:

(defun set-variable (variable value &optional no-save)
  "Set the VARIABLE to VALUE, and return VALUE.

  Save to `custom-file' if NO-SAVE is nil."
  (customize-set-variable variable value)
  (when (and (not no-save)
             (file-writable-p custom-file))
    (with-temp-buffer
      (insert-file-contents custom-file)
      (goto-char (point-min))
      (while (re-search-forward
              (format "^[\t ]*[;]*[\t ]*(setq %s .*)" variable)
              nil t)
        (replace-match (format "(setq %s '%s)" variable value) nil nil))
      (write-region nil nil custom-file)
      (message "Saved %s (%s) to %s" variable value custom-file))))


;; Font
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun icon-displayable-p ()
  "Return non-nil if icons are displayable."
  (and my-icon
       (or (display-graphic-p) (daemonp))
       (or (featurep 'all-the-icons)
           (require 'all-the-icons nil t))))

;; UI
(defun childframe-workable-p ()
  "Whether childframe is workable."
  (or (not (or noninteractive
               emacs-basic-display
               (not (display-graphic-p))))
      (daemonp)))

;; Themes
(defun default--theme-name (theme)
  "Return internal THEME name."
  (or (alist-get theme theme-alist) theme 'doom-one))

(defun theme-enable-p (theme)
  "The THEME is enabled or not."
  (and theme
       (not (memq my-theme '(auto random system)))
       (memq (default--theme-name theme) custom-enabled-themes)))

(defun compatible-theme-p (theme)
  "Check if the THEME is compatible. THEME is a symbol."
  (or (memq theme '(auto random system))
      (string-prefix-p "doom" (symbol-name (default--theme-name theme)))))

(defun default--load-theme (theme)
  "Disable others and enable new one."
  (when-let ((theme (default--theme-name theme)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(defun load-system-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (default--load-theme (alist-get appearance centaur-system-themes)))

(defun load-random-theme ()
  "Load the random theme."
  (interactive)
  (let* ((themes (mapcar #'cdr centaur-theme-alist))
         (theme (nth (random (length themes)) themes)))
    (if (eq theme my-theme)
        (load-random-theme)
      (default--load-theme theme))))

(defun my-load-theme (theme &optional no-save)
  "Load color THEME. Save to `custom-file' if NO-SAVE is nil."
  (interactive
   (list
    (intern
     (ivy-read "Load theme: "
               `(auto
                 random
                 system
                 ,@(mapcar #'car theme-alist))
               :preselect (symbol-name my-theme)))))

  ;; Disable system theme
  (when (bound-and-true-p auto-dark-mode)
    (setq auto-dark--last-dark-mode-state 'unknown)
    (auto-dark-mode -1))

  (pcase theme
    ('auto
     ;; Time-switching themes
     (use-package circadian
       :ensure t
       :functions circadian-setup
       :custom (circadian-themes auto-themes)
       :init (circadian-setup)))
    ('system
     ;; System-appearance themes
     (use-package auto-dark
       :ensure t
       :diminish
       :init
       (setq auto-dark-light-theme (alist-get 'light system-themes)
             auto-dark-dark-theme (alist-get 'dark system-themes))
       (when (and sys/macp (not (display-graphic-p)))
         (setq auto-dark-detection-method 'osascript))
       (auto-dark-mode 1)))
    ('random
     (load-random-theme))
    (_
     (default--load-theme theme)))

  ;; Set option
  (set-variable 'my-theme theme no-save))


;; Pakcage repository (ELPA)
(defun set-package-archives (archives &optional refresh async no-save)
  "Set the package archives (ELPA).

REFRESH is non-nil, will refresh archive contents.
ASYNC specifies whether to perform the downloads in the background.
Save to `custom-file' if NO-SAVE is nil."
  (interactive
   (list
    (intern
     (ivy-read "Select package archives: "
               (mapcar #'car package-archives-alist)
               :preselect (symbol-name package-archives)))))
  ;; Set option
  (set-variable 'package-archives archives no-save)

  ;; Refresh if need
  (and refresh (package-refresh-contents async))

  (message "Set package archives to `%s'" archives))

;; File and buffer
(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer")))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; Reload configurations
(defun reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file))

;; Frame
(defvar frame--geometry nil)
(defun frame--save-geometry ()
  "Save current frame's geometry."
  (setq frame--geometry
        `((left   . ,(frame-parameter nil 'left))
          (top    . ,(frame-parameter nil 'top))
          (width  . ,(frame-parameter nil 'width))
          (height . ,(frame-parameter nil 'height))
          (fullscreen))))

(defun frame--fullscreen-p ()
  "Returns Non-nil if the frame is fullscreen."
  (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth)))

(defun frame-maximize ()
  "Maximize the frame."
  (interactive)
  (frame--save-geometry)
  (unless (eq (frame-parameter nil 'fullscreen) 'maximized)
    (set-frame-parameter nil 'fullscreen 'maximized)))

(defun frame-restore ()
  "Restore the frame's size and position."
  (interactive)
  (modify-frame-parameters nil frame--geometry))

(defun frame-left-half ()
  "Put the frame to the left-half."
  (interactive)
  (unless (frame--fullscreen-p)
    (frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (/ (nth 2 attr) 2) 20))
           (height (- (nth 3 attr) 30))
           (left (nth 0 attr))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun frame-right-half ()
  "Put the frame to the right-half."
  (interactive)
  (unless (frame--fullscreen-p)
    (frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (/ (nth 2 attr) 2) 20))
           (height (- (nth 3 attr) 30))
           (left (+ (nth 0 attr) width 20))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun frame-top-half ()
  "Put the frame to the top-half."
  (interactive)
  (unless (frame--fullscreen-p)
    (frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (nth 2 attr) 20))
           (height (- (/ (nth 3 attr) 2) 30))
           (left (nth 0 attr))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun frame-bottom-half ()
  "Put the frame to the bottom-half."
  (interactive)
  (unless (frame--fullscreen-p)
    (frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (nth 2 attr) 20))
           (height (- (/ (nth 3 attr) 2) 30))
           (left (nth 0 attr))
           (top (+ (nth 1 attr) height 30)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(provide 'init-funcs)

;;; init-funcs.el ends here
