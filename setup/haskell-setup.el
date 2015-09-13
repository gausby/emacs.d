;;; haskell-setup.el --- setup for haskell-mode

;;; Commentary:
;;

;;; Code:
(require 'haskell-mode)
(require 'inf-haskell)
(require 'flycheck-haskell)

(setq haskell-program-name "ghci")

(with-eval-after-load "haskell-mode"
  (custom-set-variables
   '(haskell-mode-hook
     '(turn-on-haskell-indentation
       turn-on-haskell-doc))))

(with-eval-after-load "flycheck"
  (with-eval-after-load "haskell"
    (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)))

(provide 'haskell-setup)

;;; haskell-setup.el ends here
