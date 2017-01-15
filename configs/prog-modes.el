(el-get-bundle 'company-mode
  (require 'company)
  (setq company-idle-delay 0.7
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-flip-when-above t)
  (global-company-mode 1))

(el-get-bundle 'flycheck)

(el-get-bundle 'paredit
  (progn
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)))

(el-get-bundle 'rainbow-delimiters
  (progn
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)))
