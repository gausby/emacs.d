;;; system-setup.el --- system setup

;;; Commentary:
;;

;;; Code:

;; attempt to setup path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(provide 'system-setup)

;;; system-setup.el ends here
