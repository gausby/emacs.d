;;; navigation-setup.el --- setup for everything navigation

;;; Commentary:
;;  Setup for the various navigation modes
;;    * Use avy to navigate to chars or lines in buffer

;;; Code:
(require 'avy)

(global-set-key (kbd "C-c SPC") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "C-c C-SPC") 'avy-goto-line)

(provide 'navigation-setup)

;;; navigation-setup.el ends here
