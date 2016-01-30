;;; elm-setup.el --- setup for elm

;;; Commentary:
;;  Setup for elm which is a minimal version of evil that can be toggled

;;; Code:
(require 'elm-mode)
(require 'company)

(add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
(add-to-list 'company-backends 'company-elm)

(provide 'elm-setup)

;;; elm-setup.el ends here
