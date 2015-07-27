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
;;  * The calendar does not know about any holidays, except my own

;;; Code:
(require 'calendar)

(setq diary-file "~/.diary")

(calendar-set-date-style 'european)

(setq calendar-week-start-day 1
      calendar-view-diary-initially-flag t
      calendar-mark-diary-entries-flag t)

(setq holiday-christian-holidays nil
      holiday-oriental-holidays nil
      holiday-bahai-holidays nil
      holiday-islamic-holidays nil
      holiday-hebrew-holidays nil)

(setq holiday-general-holidays
      '((holiday-fixed 12 31 "Party")
        (holiday-fixed  1  1 "Hangover")))

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
