;;; haskell-setup.el --- setup for haskell-mode

;;; Commentary:
;;
;;  * should work with stack based haskell install, ghci is told to use the
;;    stack version `stack ghci`
;;  * enable company for haskell
;;  * enable flycheck
;;; Code:
(require 'haskell-mode)

(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)

(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)

(setq haskell-process-type 'stack-ghci)
(setq haskell-process-path-ghci "stack")
(setq haskell-process-args-ghci "ghci")

(require 'flycheck)
(require 'flycheck-haskell)
(add-hook 'haskell-mode-hook 'flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-haskell-configure)

(require 'company)
(require 'company-ghci)
(push 'company-ghci company-backends)
(add-hook 'haskell-mode-hook 'company-mode)

(provide 'haskell-setup)

;;; haskell-setup.el ends here
