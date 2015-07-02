;;; org-setup.el --- setup for org mode

;;; Commentary:
;;  Setup for org mode. This will
;;
;;   * Enable syntax highlighting in code blocks

;;; Code:
(require 'org)

;; enable syntax highlighting in code blocks
(setq org-src-fontify-natively t)


(provide 'org-setup)

;;; org-setup.el ends here
