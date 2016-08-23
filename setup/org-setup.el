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
;; Remove the background box on headlines in the material theme
(let* ((headline `(:background nil :box nil)))
  (custom-theme-set-faces 'user
                          `(org-level-4 ((t (:height 1.0))))
                          `(org-level-3 ((t (:height 1.0))))
                          `(org-level-2 ((t (,@headline :height 1.0))))
                          `(org-level-1 ((t (,@headline :height 1.2))))
                          `(org-document-title ((t (,@headline :height 1.25 :underline nil))))
                          `(org-block-begin-line ((t (:box nil))))
                          `(org-block-end-line ((t (:box nil))))))

;; Enable syntax highlighting in code blocks
(setq org-src-fontify-natively t)

(defun mg/org-mode-hook ()
  "Hooks for web-mode"
  (org-bullets-mode 1)
  (visual-line-mode 1)
  (set-visual-wrap-column 90)
  (set-fill-column 90))
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
   (http       . t)
   (C          . t)
   (css        . t)
   (js         . t)
   (calc       . t)
   (haskell    . t)
   (latex      . t)
   (lfe        . t)
   (plantuml   . t)))

(setq org-confirm-babel-evaluate nil) ;; don't ask about evaluating code

;; @todo perhaps link the jar file so I don't need to update this variable when plantuml update
(setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/8031/plantuml.8031.jar")

;; Present
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (set-visual-wrap-column 256)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (set-visual-wrap-column 80)
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))

;; Looks
(setq org-hide-emphasis-markers t) ;; hide asterisks and slashes for bold and italics

(provide 'org-setup)

;;; org-setup.el ends here
