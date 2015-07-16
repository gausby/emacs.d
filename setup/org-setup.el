;;; org-setup.el --- setup for org mode

;;; Commentary:
;;  Setup for org mode. This will
;;
;;   * Enable syntax highlighting in code blocks
;;   * Integrate with diary-mode
;;   * Let journal use a title block as headline
;;   * Let journal headlines be in the format:
;;       July 15 2015
;;       #TAGS: Wednesday week29
;;       * Today

;;; Code:
(require 'org)
(require 'org-journal)
(require 'org-present)

;; Journal
(setq org-journal-date-prefix "#+TITLE: "
      org-journal-date-format "%B %e %Y%n#+FILETAGS: :%A:week%V:%n%n* Today"
      org-journal-time-prefix "** ")

;; Agenda
(setq org-directory "~/Dropbox/org"
      org-default-notes-file (concat org-directory "/notes.org")
      org-agenda-files (file-expand-wildcards (concat org-directory "/*.org")))

;; Integrations
(setq org-agenda-include-diary t)

;; enable syntax highlighting in code blocks
(setq org-src-fontify-natively t)

;; Present
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))

(provide 'org-setup)

;;; org-setup.el ends here
