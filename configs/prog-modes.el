(el-get-bundle 'company-mode)

(el-get-bundle 'flycheck)

(el-get-bundle 'paredit
  (progn
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)))

(el-get-bundle 'rainbow-delimiters
  (progn
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)))
