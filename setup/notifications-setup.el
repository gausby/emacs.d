;;; notifications-setup.el --- setup for notifications

;;; Commentary:
;;  Setup for notification systems such as Sauron

;;; Code:
(require 'sauron)

(setq sauron-separate-frame nil
      sauron-hide-mode-line t)

(setq sauron-watch-nicks '("gausby")
      sauron-nick-insensitivity 120)

(provide 'notifications-setup)

;;; notifications-setup.el ends here
