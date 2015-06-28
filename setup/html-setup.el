;;; html-setup.el --- setup for editing html

;;; Commentary:
;;  Setup using web-mode to edit html files and related stuff

;;; Code:
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))

(provide 'html-setup)

;;; html-setup.el ends here
