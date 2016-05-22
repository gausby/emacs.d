;;; lfe-setup.el --- setup for lfe programming environment

;;; Commentary:
;;   * enable lfe-mode for files ending in .lfe
;;   * enable rainbow delimiters and paredit mode

;;; Code:

(require 'lfe-mode)

(add-to-list 'auto-mode-alist '("\\.lfe$" . lfe-mode))

(defun mg/lfe-mode-hook ()
  (rainbow-delimiters-mode +1)
  (enable-paredit-mode))
(add-hook 'lfe-mode-hook 'mg/lfe-mode-hook)

(provide 'lfe-setup)
;;; lfe-setup.el ends here
