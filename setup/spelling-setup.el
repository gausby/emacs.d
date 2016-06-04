;;; spelling-setup.el --- setup for all things spelling

;;; Commentary:
;;

;;; Code:
(require 'flyspell-popup)

(define-key flyspell-mode-map (kbd "M-`") #'flyspell-popup-correct)

(provide 'spelling-setup)

;;; spelling-setup.el ends here
