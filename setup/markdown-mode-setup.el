;;; markdown-mode-setup.el --- setup for markdown-mode

;;; Commentary:
;;  - Setup for markdown mode, use github flavored markdown
;;  - Enable fountain mode for .fountain files
;;  - Enable visual line mode for markdown buffers

;;; Code:
(require 'markdown-mode)

(setq-default markdown-command "pandoc -S -s --self-contained -f markdown -t html5 ")

(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))

(defun mg/gfm-mode-hook ()
  (visual-line-mode +1))
(add-hook 'gfm-mode-hook 'mg/gfm-mode-hook)

(add-to-list 'auto-mode-alist '("\\.fountain$" . fountain-mode))

(provide 'markdown-mode-setup)
;;; markdown-mode-setup.el ends here
