;;; emms-setup.el --- setup for the emacs media system

;;; Commentary:
;;  Play sounds and music
;;
;;  * media folder is `~/Music/`

;;; Code:
;;(require 'emms)

(emms-all)
(emms-default-players)
(setq emms-source-file-default-directory "~/Music/")
(setq emms-info-asynchronously t)

(provide 'emms-system-setup)

;;; emms-setup.el ends here
