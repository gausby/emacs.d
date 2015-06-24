;;; god-mode-setup.el --- setup for god-mode

;;; Commentary:
;;  Setup for god-mode which is a minimal version of evil that can be toggled

;;; Code:
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)
(define-key god-local-mode-map (kbd ".") 'repeat)

;; Update the cursor based on the god mode state
(defun god-mode-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'hollow
                      'box)))

(add-hook 'god-mode-enabled-hook 'god-mode-update-cursor)
(add-hook 'god-mode-disabled-hook 'god-mode-update-cursor)

(provide 'god-mode-setup)

;;; god-mode-setup.el ends here
