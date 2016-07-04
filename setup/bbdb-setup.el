;;; bbdb-mode-setup.el --- setup for BBDB

;;; Commentary:
;;  Makes auto-completing emails and stuff a thing!
;;
;;  * Move the bbdb-file outside of the .emacs.d folder
;;  * Use UTF8 for encoding the database file
;;  * Only ask for name and email when creating new entries
;;  * Set the phone number style to free-style (I am not in or from the states)

;;; Code:
(require 'bbdb)
(setq bbdb-file "~/.bbdb.gpg")

(setq bbdb-phone-style nil
      bbdb-send-mail-style 'compose-mail
      bbdb-file-coding-system 'utf-8)

;; only ask for name and email when creating new entries
(defun mg/bbdb-create (name email)
  "Add a new entry to the bbdb database.
This is different from `bbdb-create', "
  (interactive "sName: \nsMail Address: ")
  (bbdb-create-internal name nil nil nil email nil nil nil))

(define-key bbdb-mode-map "c" 'mg/bbdb-create)
(define-key bbdb-mode-map "C" 'bbdb-create)

(provide 'bbdb-setup)

;;; bbdb-mode-setup.el ends here
