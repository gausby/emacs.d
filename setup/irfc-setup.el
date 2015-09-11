;;; irfc-setup.el --- setup for the irfc rfc reader

;;; Commentary:
;;  Setup for irfc which is a RFC reader

;;; Code:
(require 'irfc)

(setq irfc-directory "~/Documents/RFC")

(add-to-list 'auto-mode-alist
             `(,(format "%s/.*\\.txt" (regexp-quote (expand-file-name irfc-directory))) . irfc-mode))

(provide 'irfc-setup)
;;; irfc-setup.el ends here
