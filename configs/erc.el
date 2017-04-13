(el-get-bundle erc
  (erc-log-mode 1))

(el-get-bundle znc
  :description "Helper enabling erc to connect to ZNC bouncers"
  :type github :pkgname "sshirokov/ZNC.el" :features znc)


;; in chat -------------------------------------------------------------
(el-get-bundle erc-colorize
  (erc-colorize-mode 1))

(el-get-bundle erc-highlight-nicknames
  (progn
    (add-to-list 'erc-modules 'highlight-nicknames)
    (erc-update-modules)))


;; viewing logs --------------------------------------------------------
(el-get-bundle erc-view-log
  (add-to-list
   'auto-mode-alist
   `(,(format "%s.*\\.txt"
              (regexp-quote (expand-file-name erc-log-channels-directory)))
     . erc-view-log-mode)))


(with-eval-after-load 'erc
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  ;; Navigation --------------------------------------------------------
  (defun mg/erc-switch-to-buffer ()
    (interactive)
    (switch-to-buffer
     (completing-read
      "Switch to ERC buffer: "
      (delq nil
            (mapcar
             (lambda (buf)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (and (memq major-mode '(erc-mode erc-view-log-mode))
                        (buffer-name buf)))))
             (buffer-list))))))
  ;; searching logs
  (defun mg/search-erc-logs (term)
    "Search the irc logs for a given term"
    (interactive "sTerm to search for in the logs: ")
    (counsel-ag term erc-log-channels-directory))
  ;; Keybindings -------------------------------------------------------
  ;; unset keybindings
  (dolist (key '("C-c C-i" "C-c C-j" "C-c C-l"
                 "C-c C-o" "C-c C-p" "C-c C-s"
                 "C-c C-t"))
    (define-key erc-mode-map (kbd key) nil))
  ;; set keybindings
  (define-key erc-mode-map (kbd "C-c C-d") 'erc-toggle-timestamps)
  (define-key erc-mode-map (kbd "C-c C-n") 'erc-track-switch-buffer)
  (define-key erc-mode-map (kbd "C-c C-o") 'browse-url-at-point)
  (define-key erc-mode-map (kbd "C-c b") #'mg/erc-switch-to-buffer)
  ;; Mode Hook ---------------------------------------------------------
  (defun mg/erc-mode-hook ()
    (erc-spelling-mode 1)
    (erc-fill-disable)
    (visual-line-mode)
    (set-visual-wrap-column 74))
  (add-hook 'erc-mode-hook 'mg/erc-mode-hook))

(with-eval-after-load 'erc-track
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477")
        ;; ...and please stop complaining about my key bindings!
        erc-track-enable-keybindings nil))

(with-eval-after-load 'erc-stamp
  (setq erc-insert-timestamp-function 'erc-insert-timestamp-left
        erc-timestamp-format "%H:%M:%S "
        erc-hide-timestamps t))

(with-eval-after-load 'erc-fill
  (setq erc-fill-column nil))

(with-eval-after-load 'erc-log
  ;; log filter function
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
        erc-log-file-coding-system 'utf-8))

(with-eval-after-load 'erc-view-log
  (setq erc-view-log-my-nickname-match '("gausby"))
  ;; Keybindings -------------------------------------------------------
  (define-key erc-view-log-mode-map (kbd "n") 'next-line)
  (define-key erc-view-log-mode-map (kbd "p") 'previous-line)
  (define-key erc-view-log-mode-map (kbd "o") 'occur)
  (define-key erc-view-log-mode-map (kbd "M-n") 'erc-view-log-next-mention)
  (define-key erc-view-log-mode-map (kbd "M-p") 'erc-view-log-previous-mention)
  (define-key erc-view-log-mode-map (kbd "q") 'bury-buffer)
  ;; Mode Hook ---------------------------------------------------------
  (defun mg/erc-view-log-mode-hook ()
    (visual-line-mode)
    (set-visual-wrap-column 74))
  (add-hook 'erc-view-log-mode-hook 'mg/erc-view-log-mode-hook))
