;;; popwin-setup.el --- setup for popwin-mode

;;; Commentary:
;;  Setup for how certain buffers behave

;;; Code:
(require 'popwin)
(popwin-mode 1)

(push '(elixir-mix-task-runner) popwin:special-display-config)
(push '("*Messages*" :noselect t :stick t) popwin:special-display-config)
(push '("*compilation*" :height 25 :noselect t :stick t) popwin:special-display-config)
(push '("*mix*" :height 25 :noselect t) popwin:special-display-config)
(push '("*alchemist message*" :height 20 :noselect t) popwin:special-display-config)

(provide 'popwin-setup)

;;; popwin-setup.el ends here
