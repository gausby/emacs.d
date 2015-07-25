;;; erc-setup.el --- setup for erc and znc

;;; Commentary:
;;  Setup for erc and znc
;;
;;  * Enable spelling mode for erc buffers
;;  * Disable fill mode, makes it easier to search using erc-occur
;;  * Keybindings should be prefixed with `C-c i`, i for "irc"
;;  * Connect to all servers using `C-c i c a`, connect to only one using `C-c i c c`
;;  * Switch to Next unread erc buffer on `C-c i n` and back to the previous
;;    non erc buffer when there is no more unread buffers (n for next)
;;  * Disable/Enable track mode with `C-c i t` (t for track)
;;  * Disable/Enable timestamps with `C-c i d` (d for 'date-time')
;;  * Search in erc buffers with `C-c i s` (s for search)

;;; Code:
(require 'erc)
(require 'erc-stamp)

(erc-colorize-mode 1)
(erc-spelling-mode 1)

;; in chat -----------------------------------------------------------------
(defun my-erc-mode-hook ()
  (erc-fill-disable)
  (visual-line-mode))
(add-hook 'erc-mode-hook 'my-erc-mode-hook)

(setq erc-timestamp-only-if-changed-flag t
      erc-insert-timestamp-function 'erc-insert-timestamp-right
      erc-fill-column nil
      erc-timestamp-format "[%H:%M]"
      erc-hide-timestamps t)

;; messages
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; tracking ----------------------------------------------------------------
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477")
      ;; ...and please stop complaining about my key bindings!
      erc-track-enable-keybindings nil)

;; Get rid of the iswitchb obsolete message when hitting C-c C-b
(eval-after-load "erc" '(define-key erc-mode-map (kbd "C-c C-b") nil))

;; define keybindings
(define-key global-map (kbd "C-c i c a") 'znc-all)
(define-key global-map (kbd "C-c i c c") 'znc-erc)
(define-key global-map (kbd "C-c i n") 'erc-track-switch-buffer)
(define-key global-map (kbd "C-c i t") 'erc-track-mode)
(define-key global-map (kbd "C-c i d") 'erc-toggle-timestamps)
(define-key global-map (kbd "C-c i s") 'erc-occur)

(provide 'erc-setup)

;;; erc-setup.el ends here
