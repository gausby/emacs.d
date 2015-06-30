;;; diary-mode-setup.el --- setup for diary-mode

;;; Commentary:
;;  dear diary-mode

;;; Code:
(require 'calendar)

(setq diary-file "~/.diary")

;; monday is the first day of the week
(setq calendar-week-start-day 1)

;; Let emacs know where we are
(setq calendar-latitude +55.0)
(setq calendar-longitude +12.0)
(setq calendar-location-name "Copenhagen, DK")

(provide 'diary-mode-setup)

;;; diary-mode-setup.el ends here
