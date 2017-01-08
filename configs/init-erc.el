;; in chat -----------------------------------------------------------
(defun mg/erc-mode-hook ()
  (erc-spelling-mode 1)
  (erc-fill-disable)
  (visual-line-mode)
  (set-visual-wrap-column 90))
(add-hook 'erc-mode-hook 'mg/erc-mode-hook)

(setq erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-fill-column nil
      erc-timestamp-format "%H:%M:%S "
      erc-hide-timestamps t)

;; messages
(setq erc-hide-list '("JOIN" "PART" "QUIT"))


;; tracking -----------------------------------------------------------
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477")
      ;; ...and please stop complaining about my key bindings!
      erc-trcak-enable-keybindings nil)


;; log to disk --------------------------------------------------------
(erc-log-mode 1)

(defun mg/erc-log-filter (topic)
  "Filter out system messges from the logs"
  (if (equal (string-match-p "^\[[:digit:]: ]+\\*\\*\\*[[:space:]]" topic) nil)
      topic
      ""))

(setq erc-enable-logging 'erc-log-all-but-server-buffers
      erc-log-channels-directory "~/.erc/logs/"
      erc-generate-log-file-name-function 'erc-generate-log-file-name-with-date
      erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil
      erc-log-filter-function 'mg/erc-log-filter
      erc-log-write-after-insert t
      erc-log-write-after-send t
      erc-log-file-coding-system 'utf-8)

;; searching logs
(defun mg/search-erc-logs (term)
  "Search the irc logs for a given term"
  (interactive "sTerm to search for in the logs: ")
  (counsel-ag term erc-log-channels-directory))


;; Keybindings ---------------------------------------------------------
;(define-prefix-command 'irc-global-map)
;; unset keybindings
(dolist (key '("C-c C-i" "C-c C-j" "C-c C-l"
               "C-c C-o" "C-c C-p" "C-c C-s"
               "C-c C-t"))
  (define-key erc-mode-map (kbd key) nil))

(define-key erc-mode-map (kbd "C-c C-d") 'erc-toggle-timestamps)
(define-key erc-mode-map (kbd "C-c C-b") 'erc-track-switch-buffer)
