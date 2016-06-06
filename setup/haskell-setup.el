;;; haskell-setup.el --- setup for haskell-mode

;;; Commentary:
;;
;;  * Use intero mode for Haskell development

;;; Code:
(require 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)

(provide 'haskell-setup)

;;; haskell-setup.el ends here
