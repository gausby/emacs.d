;;; emacs-lisp-setup.el --- setup for emacs-lisp

;;; Commentary:
;;  emacs-lisp programming environment

;;; Code:
(require 'macrostep)

(defun t-emacs-lisp-mode-hook ()
  (eldoc-mode)
  (rainbow-delimiters-mode +1)
  (rainbow-mode +1)
  (flycheck-mode +1)
  (enable-paredit-mode))

(add-hook 'emacs-lisp-mode-hook 't-emacs-lisp-mode-hook)

;; disable checkdoc for emacs lisp, it is so annoying
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)

(provide 'emacs-lisp-setup)

;;; emacs-lisp-setup.el ends here
