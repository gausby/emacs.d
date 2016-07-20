;;; mail-setup.el --- setup for reading and sending email in emacs

;;; Commentary:
;; Setup for the notmuch reader and emacs mail for sending
;; This will:
;;
;;   * Add a hook for always signing messages
;;   * Make notmuch hello screen less cluttered
;;   * Configure saved searches for my notmuch setup
;;   * Use jump search (press `j` in the notmuch-hello) to navigate saved notmuch searches
;;     - i = everything in the inbox
;;     - u = unread messages
;;     - f = flagged messages + not replied
;;     - F = all flagged messages
;;     - d = drafts
;;     - s = sent messages
;;     - t = display today (email received within the last 24 hours from now
;;     - m = mailing-lists (unread)
;;     - M = mailing-lists archive
;;     - c = display who connect to me on various social media sites
;;   * In search lists `u` can be used to display messages taged with *unread*
;;
;;  I rely on a bash script that tag mail in my notmuch mail database. I should perhaps
;;  open source this script at some point.

;;; Code:
(require 'notmuch)

(setq notmuch-hello-sections
      '(notmuch-hello-insert-search
        notmuch-hello-insert-saved-searches
        notmuch-hello-insert-alltags))

(setq notmuch-saved-searches
      '((:name "unread" :query "tag:unread and not (tag:mailing-list or tag:github)" :key "u")
        (:name "inbox" :query "tag:inbox" :key "i")
        (:name "flagged and not replied" :query "tag:flagged and not tag:replied" :key "f")
        (:name "flagged" :query "tag:flagged" :key "F")
        (:name "drafts" :query "tag:draft" :key "d")
        (:name "sent" :query "tag:sent" :key "s")
        (:name "today" :query "tag:inbox and date:-24h..now and not tag:mailing-list" :key "t")
        (:name "mailing-lists" :key "m"
               :query "tag:mailing-list and tag:unread" :sort-order 'newest-first
               :count-query "tag:mailing-list and tag:unread")
        (:name "mailing-list archive" :key "M"
               :query "tag:mailing-list" :sort-order 'newest-first)
        (:name "github" :key "g"
               :query "tag:github and tag:unread" :sort-order 'newest-first
               :count-query "tag:github and tag:unread")
        (:name "github archive" :key "G"
               :query "tag:github" :sort-order 'newest-first)
        (:name "connections" :key "c"
               :query "tag:friend-request date:-7d..now"
               :count-query "tag:friend-request date:-24h..now")))

;; key-bindings in search lists
(define-key notmuch-search-mode-map "u"
  (lambda (&optional beg end)
    "display only unread messages for the current search view"
    (interactive (notmuch-search-interactive-region))
    (notmuch-search-filter-by-tag "unread")))

;; mode hooks
(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

(provide 'mail-setup)

;;; mail-setup.el ends here
