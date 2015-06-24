;;; company-setup.el --- completion setup

;;; Commentary:
;;

;;; Code:

(require 'company)

(setq company-idle-delay 0.1)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-flip-when-above t)

(global-company-mode 1)

(provide 'company-setup)

;;; company-setup.el ends here
