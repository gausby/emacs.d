;;; ace-setup.el --- setup for everything ace

;;; Commentary:
;;  Setup for the various ace modes

;;; Code:
(require 'ace-jump-mode)

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-c C-SPC") 'ace-jump-line-mode)

(provide 'ace-setup)

;;; ace-setup.el ends here
