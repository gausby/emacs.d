;;; browser-setup.el --- setup for web browsing

;;; Commentary:
;;  * Set the default emacs browser to eww

;;; Code:

(setq browse-url-browser-function 'eww-browse-url)

(provide 'browser-setup)

;;; browser-setup.el ends here
