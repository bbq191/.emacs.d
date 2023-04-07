;;; init-program,el --initial program utils --
;;; Commentary:
;;; Code:

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :config
  (global-set-key (kbd "M-n") #'flymake-goto-next-error)
  (global-set-key (kbd "M-p") #'flymake-goto-prev-error))


(provide 'program)
;;; program.el ends here

