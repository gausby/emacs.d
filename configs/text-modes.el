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
  (setq org-src-fontify-natively t     ;; highlight code blocks
        org-confirm-babel-evaluate nil ;; don't ask about code eval
        ;; hide asterisks and slashes for bold and italics
        org-hide-emphasis-markers t
        ;; make C-a and C-e jump to logical positions
        org-special-ctrl-a/e t
        ;; make kill-line delete logical parts of the line (headline
        ;; first, then tags, etc)
        org-special-ctrl-k t)
  ;; org-capture and org-agenda
  (setq org-directory "~/Notes/"
        org-default-notes-file "capture.org"
        org-capture-templates '(("i" "Inbox" entry (file+headline "capture.org" "Inbox") "* %?\n %i\n ")))
  (let ((default-directory org-directory)
        (location-format "archives/%Y-%W-archive.org::* Archived"))
    (setq org-agenda-files (list (expand-file-name "capture.org"))
          org-archive-location (expand-file-name (format-time-string location-format))))
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
      (set-visual-wrap-column 90)
      (set-fill-column 90))))
(define-key ctl-x-map (kbd "SPC") 'org-capture)

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
