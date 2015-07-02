;;; markdown-mode-setup.el --- setup for markdown-mode

;;; Commentary:
;;  Setup for markdown mode

;;; Code:
(require 'markdown-mode)

(setq-default markdown-command "pandoc -S -s --self-contained -f markdown -t html5 ")

(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))

(provide 'markdown-mode-setup)
;;; markdown-mode-setup.el ends here
