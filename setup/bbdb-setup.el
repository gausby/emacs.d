;;; diary-mode-setup.el --- setup for diary-mode

;;; Commentary:
;;  Makes auto-completing emails and stuff a thing!
;;
;;  * Move the bbdb-file outside of the .emacs.d folder
;;  * Set the phone number style to free-style (I am not in or from the states)

;;; Code:
(require 'bbdb)
(setq bbdb-file "~/.bbdb.gpg")

(setq bbdb-phone-style nil
      bbdb-send-mail-style 'compose-mail)

(provide 'bbdb-setup)

;;; diary-mode-setup.el ends here
