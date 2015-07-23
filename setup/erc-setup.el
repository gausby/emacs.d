;;; erc-setup.el --- setup for erc and znc

;;; Commentary:
;;  Setup for erc and znc
;;
;;  * Keybindings should be prefixed with `C-c i`, i for "irc"
;;  * Switch to Next unread erc buffer on `C-c i n` and back to the previous
;;    non erc buffer when there is no more unread buffers (n for next)
;;  * Disable/Enable track mode with `C-C i t` (t for track)

;;; Code:
(require 'erc)

(erc-colorize-mode 1)

;; in chat -----------------------------------------------------------------
;; time stamps
(setq erc-timestamp-only-if-changed-flag t
      erc-timestamp-format "%H:%M "
      erc-fill-prefix "        "
      erc-insert-timestamp-function 'erc-insert-timestamp-left)
;; messages
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; tracking ----------------------------------------------------------------
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477")
      ;; ...and please stop complaining about my key bindings!
      erc-track-enable-keybindings nil)

;; Get rid of the iswitchb obsolete message when hitting C-c C-b
(eval-after-load "erc" '(define-key erc-mode-map (kbd "C-c C-b") nil))

;; Switch to unread buffer
(define-key global-map (kbd "C-c i n") 'erc-track-switch-buffer)
(define-key global-map (kbd "C-c i t") 'erc-track-mode)

(provide 'erc-setup)

;;; erc-setup.el ends here
