;;; yasnippet-setup.el --- setup for the yasnippet system

;;; Commentary:
;;  * Enable yasnippet globally

;;; Code:
(require 'yasnippet)

;; make sure snippets are not fetched from yasnippets official snippet set,
;; as it will conflict with my own snippets
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))

(yas-global-mode +1)

(provide 'yasnippet-setup)
;;; yasnippet-setup.el ends here
