;;; mail-setup.el --- setup for reading and sending email in emacs

;;; Commentary:
;; Setup for the notmuch reader and emacs mail for sending
;; This will:
;;
;;   * Add a hook for always signing messages
;;   * Configure saved searches for my notmuch setup
;;   * Use jump search (press `j` in the notmuch-hello) to navigate saved notmuch searches
;;     - i = everything in the inbox
;;     - u = unread messages
;;     - f = flagged messages
;;     - d = drafts
;;     - s = sent messages
;;     - t = display today (email received within the last 24 hours from now
;;     - m = mailing-lists

;;; Code:
(require 'notmuch)

(setq notmuch-saved-searches
      '((:name "unread" :query "tag:unread and not tag:mailing-list" :key "u")
        (:name "inbox" :query "tag:inbox" :key "i")
        (:name "flagged" :query "tag:flagged" :key "f")
        (:name "drafts" :query "tag:draft" :key "d")
        (:name "sent" :query "tag:sent" :key "s")
        (:name "today" :query "tag:inbox and date:-24h..now and not tag:mailing-list" :key "t")
        (:name "mailing-lists" :key "m"
               :query "tag:mailing-list" :sort-order 'newest-first
               :count-query "tag:mailing-list and tag:unread")))

;; mode hooks
(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

(provide 'mail-setup)

;;; mail-setup.el ends here
