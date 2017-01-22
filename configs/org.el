(el-get-bundle org-mode)
(el-get-bundle org-bullets)

(with-eval-after-load 'org
  (setq org-src-fontify-natively t     ;; highlight code blocks
        org-confirm-babel-evaluate nil ;; don't ask about code eval
        ;; hide asterisks and slashes for bold and italics
        org-hide-emphasis-markers t)
  (defun mg/org-mode-hook ()
      "Hooks for org-mode"
      (org-bullets-mode 1)
      (visual-line-mode 1)
      (set-visual-wrap-column 90)
      (set-fill-column 90))
  (add-hook 'org-mode-hook 'mg/org-mode-hook))
