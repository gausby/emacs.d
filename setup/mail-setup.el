;;; mail-setup.el --- setup for reading and sending email in emacs

;;; Commentary:
;; Setup for the notmuch reader and emacs mail for sending
;; This will:
;;
;;   * Add a hook for always signing messages

;;; Code:
(require 'notmuch)

(setq notmuch-saved-searches
      '((:name "inbox" :query "tag:inbox" :key "i")
        (:name "unread" :query "tag:unread" :key "u")
        (:name "flagged" :query "tag:flagged" :key "f")
        (:name "sent" :query "tag:sent" :key "t")
        (:name "drafts" :query "tag:draft" :key "d")
        (:name "all mail" :query "*" :key "a")
        (:name "today" :query "date:today..now")))

;; mode hooks
(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

(provide 'mail-setup)

;;; mail-setup.el ends here
