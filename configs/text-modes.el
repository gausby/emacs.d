;;
;; Generally for text-modes
;;
(defun my-text-mode-hook ()
  (flyspell-mode 1))
(add-hook 'text-mode-hook 'my-text-mode-hook)


;;
;; Olivetti
;;
(el-get-bundle olivetti
  :type github :pkgname "rnkn/olivetti"
  :features olivetti)


;;
;; Markdown
;;
(el-get-bundle markdown-mode
  :prepare (progn
              (add-to-list 'auto-mode-alist '("\\.\\(md\\|mdown\\|markdown\\)\\'" . markdown-mode))
              (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))))


;;
;; Fountain
;;
(el-get-bundle fountain-mode
  :type github :pkgname "rnkn/fountain-mode/"
  :features fountain-mode)
(with-eval-after-load 'fountain-mode
  (define-key fountain-mode-map (kbd "C-c SPC") #'imenu)
  (add-hook 'fountain-mode-hook (lambda ()
      (text-scale-set +1)
      (setq-local olivetti-body-width 68)
      (olivetti-mode 1))))


;;
;; org-mode
;;
(el-get-bundle org-mode)
(el-get-bundle org-bullets)

(with-eval-after-load 'org
  ;; Navigation
  ;; Make C-a and C-e jump to logical positions and make kill-line
  ;; delete logical parts of the line (headline first, then tags, etc)
  (setq org-special-ctrl-a/e t
        org-special-ctrl-k t)
  ;; Code blocks and code evaluation
  ;; Use the font lock from the given major mode to highlight the code
  ;; block and don not ask for confirmation when evaluating code
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil)
  ;; setup shell for use in code blocks
  (require 'ob-shell)
  (add-to-list 'org-babel-load-languages '(shell . t))
  ;; org-capture and org-agenda
  (setq org-directory "~/Notes/"
        org-default-notes-file "capture.org"
        org-capture-templates '(("i" "Inbox"
                                 entry (file+headline "capture.org" "Inbox")
                                 "* %?\n %i\n ")
                                ("j" "Journal"
                                 entry (file+datetree "journal.org")
                                 "* %U\n%?")))
  (let ((default-directory org-directory)
        (location-format "archive/%Y-%W-archive.org::* From %s"))
    (setq org-agenda-files (list (expand-file-name "capture.org")
                                 (expand-file-name "private.org")
                                 (expand-file-name "work.org"))
          org-refile-targets '(("private.org" :maxlevel . 2)
                               ("work.org" :maxlevel . 2))
          org-archive-location (expand-file-name (format-time-string location-format))
          org-id-locations-file (expand-file-name ".org-id-locations")))
  ;; Item state changes and log drawer
  (setq org-log-into-drawer t
        org-log-done 'time
        org-log-reschedule 'note)
  ;; Helpers -----------------------------------------------------------
  (defun mg/org-decorate-nodes-with-ids ()
    "Add ID properties to nodes in the current file which
does not already have one."
    (interactive)
    (org-map-entries 'org-id-get-create))
  ;; copy the id of a node to the kill-ring (generate id if nonexistent)
  (defun mg/org-copy-node-id ()
    (interactive)
    (let ((temporary-id (funcall 'org-id-get-create)))
      (kill-new temporary-id)
      (message "Copied %s to kill-ring" temporary-id)))
  ;; Advices -----------------------------------------------------------
  ;; Preserve top level headings when archiving to a file
  ;; http://orgmode.org/worg/org-hacks.html#orgheadline59
  (defadvice org-archive-subtree (around my-org-archive-subtree activate)
    (let ((org-archive-location
           (if (save-excursion (org-back-to-heading)
                               (> (org-outline-level) 1))
               (concat (car (split-string org-archive-location "::"))
                       "::* "
                       (car (org-get-outline-path)))
             org-archive-location)))
      ad-do-it))
  ;; Mode hook ---------------------------------------------------------
  (add-hook 'org-mode-hook (lambda ()
      (org-bullets-mode 1)
      (visual-line-mode 1)
      (org-indent-mode 1)
      (set-fill-column 90)
      ;; (set-visual-wrap-column 90)
      ;; Keybindings
      (local-set-key (kbd "C-c w") 'mg/org-copy-node-id))))

(with-eval-after-load 'org-agenda
  (setq org-agenda-start-on-weekday nil))

(define-key ctl-x-map (kbd "n") 'org-capture)
(define-key ctl-x-map (kbd "a") 'org-agenda)


;;
;; IRFC
;;
(el-get-bundle irfc :depends (cl-lib))
(with-eval-after-load 'irfc
  (setq irfc-directory "~/Documents/RFC"
        irfc-assoc-mode t)
  (add-to-list 'auto-mode-alist
               `(,(format "%s/.*\\.txt" (regexp-quote (expand-file-name irfc-directory))) . irfc-mode))
  ;; font faces
  (custom-theme-set-faces 'user
      ;; structure
      `(irfc-title-face ((t (:foreground "#eceff1" :weight bold :height 1.2))))
      ;; table of contents
      `(irfc-table-item-face ((t (:foreground "#b0bec5" :weight bold))))
      ;; sections
      `(irfc-head-name-face ((t (:foreground "#b0bec5" :weight bold))))
      `(irfc-head-number-face ((t (:foreground "#8a8a8a"))))
      ;; miscellaneous
      `(irfc-requirement-keyword-face ((t (:foreground "#f36c60"))))
      `(irfc-rfc-number-face ((t (:foreground "#fff59d")))))
  ;; mode hook
  (add-hook 'irfc-mode-hook (lambda ()
      (god-local-mode 1)
      (setq-local olivetti-body-width 72)
      (olivetti-mode 1)
      ;; disable custom quit buffer, custom scrolling, and other
      ;; keybinding oddities
      (dolist (key '("q" "e" "J" "K" "," "." "P" "N" "H" "L" ">" "<"))
        (local-set-key (kbd key) nil))
      (local-set-key (kbd "M-p") 'irfc-head-prev)
      (local-set-key (kbd "M-n") 'irfc-head-next))))
