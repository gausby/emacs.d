;;; notifications-setup.el --- setup for notifications

;;; Commentary:
;;  Setup for notification systems such as Sauron

;;; Code:
(require 'sauron)

(setq sauron-separate-frame nil
      sauron-hide-mode-line t)

(setq sauron-watch-nicks '("gausby")
      sauron-nick-insensitivity 60)

;; sauron-mode-hook
(defun mg/sauron-mode-hook ()
  (text-scale-set -3))
(add-hook 'sauron-mode-hook 'mg/sauron-mode-hook)

;; Key bindings
(global-set-key (kbd "C-c n n") 'sauron-toggle-hide-show)
(global-set-key (kbd "C-c n c") 'sauron-clear)

(provide 'notifications-setup)

;;; notifications-setup.el ends here
