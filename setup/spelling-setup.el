;;; spelling-setup.el --- setup for all things spelling

;;; Commentary:
;;  - use aspell for spellchecking
;;  - use flyspell popup to display corrections in flyspell mode

;;; Code:
(require 'flyspell)
(require 'flyspell-popup)

(if (executable-find "aspell")
  (progn (setq ispell-program-name "aspell")
         (setq flyspell-issue-message-flag nil))
  (message "Please install aspell"))

(define-key flyspell-mode-map (kbd "M-`") #'flyspell-popup-correct)

(provide 'spelling-setup)

;;; spelling-setup.el ends here
