;;; git-setup.el --- setup for git

;;; Commentary:
;;  Setup for magit and packages that setup Git workflow

;;; Code:
(global-set-key (kbd "C-c m") 'magit-status)

(add-hook 'text-mode-hook 'my-text-mode-hook)
(defun my-text-mode-hook ()
  (flyspell-mode 1))

(provide 'git-setup)

;;; git-setup.el ends here
