;;; elm-setup.el --- setup for elm

;;; Commentary:
;;  Setup for elm which is a super sweet language for building UI apps

;;; Code:
(require 'elm-mode)
(require 'company)

(add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
(add-to-list 'company-backends 'company-elm)

(provide 'elm-setup)

;;; elm-setup.el ends here
