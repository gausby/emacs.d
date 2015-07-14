;;; mail-setup.el --- setup for reading and sending email in emacs

;;; Commentary:
;; Setup for the notmuch reader and emacs mail for sending
;; This will:
;;
;;   * Add a hook for always signing messages
;;   * Configure saved searches for my notmuch setup

;;; Code:
(require 'notmuch)

(setq notmuch-saved-searches
      '((:name "unread" :query "tag:unread and not tag:mailing-list" :key "u")
        (:name "inbox" :query "tag:inbox" :key "i")
        (:name "flagged" :query "tag:flagged" :key "f")
        (:name "sent" :query "tag:sent" :key "t")
        (:name "today" :query "tag:inbox and date:-24h..now and not tag:mailing-list" :key "c")
        (:name "drafts" :query "tag:draft" :key "d")
        (:name "all mail" :query "*" :key "a")
        (:name "mailing-lists" :query "tag:mailing-list" :count-query "tag:mailing-list and tag:unread" :key "m")))

;; mode hooks
(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

(provide 'mail-setup)

;;; mail-setup.el ends here
