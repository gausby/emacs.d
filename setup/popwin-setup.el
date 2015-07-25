;;; popwin-setup.el --- setup for popwin-mode

;;; Commentary:
;;  Setup for how certain buffers behave

;;; Code:
(require 'popwin)
(popwin-mode 1)

(push '(elixir-mix-task-runner) popwin:special-display-config)
(push '("*Messages*" :noselect t :stick t) popwin:special-display-config)
(push '("*compilation*" :height 25 :noselect t :stick t) popwin:special-display-config)
(push '("*alchemist-test-report*" :height 25 :stick t) popwin:special-display-config)
(push '("*alchemist message*" :height 20 :noselect t) popwin:special-display-config)
(push '("*ag search*" :height 20 :dedicated t) popwin:special-display-config)

(define-key global-map (kbd "C-x p") 'popwin:display-last-buffer)

(provide 'popwin-setup)

;;; popwin-setup.el ends here
