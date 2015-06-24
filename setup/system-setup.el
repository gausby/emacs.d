;;; system-setup.el --- system setup

;;; Commentary:
;;

;;; Code:

;; attempt to setup path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; revert files in buffers when the underlying files change
(global-auto-revert-mode t)

(provide 'system-setup)

;;; system-setup.el ends here
