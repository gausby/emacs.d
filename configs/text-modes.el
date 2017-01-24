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
