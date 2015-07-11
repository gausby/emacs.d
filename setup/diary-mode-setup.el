;;; diary-mode-setup.el --- setup for diary-mode

;;; Commentary:
;;  dear diary-mode
;;
;;  * Calendar style is European
;;  * Monday is the first day of the week
;;  * Show current diary entry when opening calendar
;;  * Set ~/.diary to the diary file
;;  * Dates with entries are marked in calendar view
;;  * Current date is marked in calendar view
;;  * Calendar view is fancy
;;  * Emacs knows about our location. It is set to Copenhagen, Denmark

;;; Code:
(require 'calendar)

(setq diary-file "~/.diary")

(calendar-set-date-style 'european)

(setq calendar-week-start-day 1
      view-diary-entries-initially t
      mark-diary-entries-in-calendar t
      number-of-diary-entries 7)

;; Let emacs know where we are
(setq calendar-latitude +55.0
      calendar-longitude +12.0
      calendar-location-name "Copenhagen, DK")

;; Hooks
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
(add-hook 'list-diary-entries-hook 'sort-diary-entries t)

(provide 'diary-mode-setup)

;;; diary-mode-setup.el ends here
