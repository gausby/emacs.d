;;; company-setup.el --- completion setup

;;; Commentary:
;;  * Set idle delay a bit long to stop a conflict with auto completing do..end
;;    blocks in files where a function starting with `do_` would complete before
;;    the auto-pair
;;  * Company mode is global

;;; Code:

(require 'company)

(setq company-idle-delay 0.7
      company-tooltip-limit 10
      company-minimum-prefix-length 2
      company-tooltip-flip-when-above t)

(global-company-mode 1)

(provide 'company-setup)

;;; company-setup.el ends here
