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
;;   * Visually replace org bullets with utf-8 bullets
;;   * Setup source blocks for execution in various langauges

;;; Code:
(require 'org)
(require 'org-agenda)
(require 'org-bullets)
(require 'org-capture)
(require 'org-journal)
(require 'org-present)

;; Looks
;; Enable syntax highlighting in code blocks
(setq org-src-fontify-natively t)

(defun mg/org-mode-hook ()
  "Hooks for web-mode"
  (org-bullets-mode 1))
(add-hook 'org-mode-hook 'mg/org-mode-hook)

;; Journal
(setq org-journal-date-prefix "#+TITLE: "
      org-journal-date-format "%B %e %Y%n#+FILETAGS: :%A:week%V:%n%n* Today"
      org-journal-time-prefix "** ")

;; Agenda
(setq org-directory "~/Dropbox/org"
      org-default-notes-file (concat org-directory "/notes.org")
      org-agenda-files (file-expand-wildcards (concat org-directory "/*.org")))

;; Capture
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/org/notes.org" "Tasks") "* TODO %?")
        ("r" "To read" entry (file+headline "~/Dropbox/org/notes.org" "Reading") "* UNREAD %?")
        ("w" "To watch" entry (file+headline "~/Dropbox/org/notes.org" "Watching") "* UNWATCHED %?")))

;; Integrations
(setq org-agenda-include-diary t)

;; Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh         . t)
   (emacs-lisp . t)
   (C          . t)
   (css        . t)
   (js         . t)
   (calc       . t)
   (haskell    . t)
   (plantuml   . t)))

(setq org-confirm-babel-evaluate nil) ;; don't ask about evaluating code

;; @todo perhaps link the jar file so I don't need to update this variable when plantuml update
(setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/8029/plantuml.8029.jar")

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
