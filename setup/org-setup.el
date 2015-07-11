;;; org-setup.el --- setup for org mode

;;; Commentary:
;;  Setup for org mode. This will
;;
;;   * Enable syntax highlighting in code blocks

;;; Code:
(require 'org)
(require 'org-present)

;; Agenda
(setq org-directory "~/Dropbox/org"
      org-default-notes-file (concat org-directory "/notes.org")
      org-agenda-files (file-expand-wildcards (concat org-directory "/*.org")))

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
