;;; expand-region-setup.el --- setup for everything expand-region

;;; Commentary:
;;  Setup for the expand region mode

;;; Code:
(require 'expand-region)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

(provide 'expand-region-setup)

;;; expand-region-setup.el ends here
