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
        org-hide-emphasis-markers t)
  (add-hook 'org-mode-hook (lambda ()
      (org-bullets-mode 1)
      (visual-line-mode 1)
      (set-visual-wrap-column 90)
      (set-fill-column 90))))


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
      (setq-local olivetti-body-width 72)
      (olivetti-mode 1)
      ;; disable quit buffer, killing it will do fine
      (local-set-key (kbd "q") nil)
      ;; disable custom scroll
      (local-set-key (kbd "e") nil)
      (local-set-key (kbd "<SPC>") nil)
      (local-set-key (kbd "J") nil)
      (local-set-key (kbd "K") nil)
      ;; end and beginning of buffer is normally M-S-> and <
      (local-set-key (kbd ",") nil)
      (local-set-key (kbd ".") nil)
      (local-set-key (kbd "M-p") 'irfc-head-prev)
      (local-set-key (kbd "M-n") 'irfc-head-next)
      (local-set-key (kbd "P") nil)
      (local-set-key (kbd "N") nil)
      (local-set-key (kbd "H") nil)
      (local-set-key (kbd "L") nil)
      (local-set-key (kbd ">") nil)
      (local-set-key (kbd "<") nil))))
