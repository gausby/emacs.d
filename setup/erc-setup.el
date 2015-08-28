;;; erc-setup.el --- setup for erc and znc

;;; Commentary:
;;  Setup for erc and znc
;;
;;  * Enable spelling mode for erc buffers
;;  * Disable fill mode, makes it easier to search using erc-occur
;;  * Keybindings should be prefixed with `C-c i`, i for "irc"
;;  * Connect to all servers using `C-c i c a`, connect to only one using `C-c i c c`
;;  * Switch to Next unread erc buffer on `C-c i b` and back to the previous
;;    non erc buffer when there is no more unread buffers (n for next)
;;  * Disable/Enable track mode with `C-c i t` (t for track)
;;  * Disable/Enable timestamps with `C-c i d` (d for 'date-time')
;;  * Search in erc buffers with `C-c i s s` (s for search)
;;  * Define a hydra for jumping to irc channels bound to `C-c i j` (j for jump)
;;  * Log conversations to ~/.erc/logs (this folder should get created manually)
;;  * Use the silver searcher to search the irc logs using `C-c i s l` (s l: search logs)

;;; Code:
(require 'erc)
(require 'erc-stamp)
(require 'erc-log)

(erc-colorize-mode 1)
(erc-spelling-mode 1)

;; in chat -----------------------------------------------------------------
(defun my-erc-mode-hook ()
  (erc-fill-disable)
  (visual-line-mode))
(add-hook 'erc-mode-hook 'my-erc-mode-hook)

(setq erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-fill-column nil
      erc-timestamp-format "%H:%M "
      erc-hide-timestamps t)

;; messages
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; log to disk
(erc-log-mode)
(setq erc-log-channels-directory "~/.erc/logs/"
      erc-generate-log-file-name-function 'erc-generate-log-file-name-with-date
      erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil
      erc-log-write-after-insert t
      erc-log-write-after-send t
      erc-log-file-coding-system 'utf-8)

(defun mg/search-erc-logs (term)
  "Search the irc logs for a given term"
  (interactive "sTerm to search for in the logs: ")
  (ag-files term '(:file-regex "[^\*]") "~/.erc/logs/"))

;; tracking ----------------------------------------------------------------
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477")
      ;; ...and please stop complaining about my key bindings!
      erc-track-enable-keybindings nil)

;; Get rid of the iswitchb obsolete message when hitting C-c C-b
(eval-after-load "erc" '(define-key erc-mode-map (kbd "C-c C-b") nil))

;; define keybindings
(define-key erc-mode-map (kbd "C-c C-i") nil)
(define-key erc-mode-map (kbd "C-c C-j") nil)
(define-key erc-mode-map (kbd "C-c C-l") nil)
(define-key erc-mode-map (kbd "C-c C-o") nil)
(define-key erc-mode-map (kbd "C-c C-p") nil)
(define-key erc-mode-map (kbd "C-c C-s") nil)
(define-key erc-mode-map (kbd "C-c C-t") nil)

(global-set-key (kbd "C-c i c a") 'znc-all)
(global-set-key (kbd "C-c i c c") 'znc-erc)
(global-set-key (kbd "C-c i b") 'erc-track-switch-buffer)
(global-set-key (kbd "C-c i t") 'erc-track-mode)
(global-set-key (kbd "C-c i d") 'erc-toggle-timestamps)
(global-set-key (kbd "C-c i s s") 'erc-occur)
(global-set-key (kbd "C-c i s l") 'mg/search-erc-logs)

(global-set-key (kbd "C-c i j") (defhydra hydra-switch-to-irc-buffer (:color blue)
                                  "Jump to IRC channel"
                                  ("e" (switch-to-buffer "#elixir-lang") "#elixir-lang")
                                  ("a" (switch-to-buffer "#emacs-elixir") "#emacs-elixir")
                                  ("c" (switch-to-buffer "#cphex") "#cphex")
                                  ("f" (switch-to-buffer "#cphftw") "#cphftw")
                                  ("j" (switch-to-buffer "#copenhagenjs") "#copenhagenjs")
                                  ("n" (switch-to-buffer "#nerdtracker") "#nerdtracker")
                                  ("g" (switch-to-buffer "#erlang") "#erlang")
                                  ("o" (switch-to-buffer "#erlounge") "#erlounge")
                                  ("l" (switch-to-buffer "#labitat") "#labitat")
                                  ("s" (switch-to-buffer "#stackvm") "#stackvm")))

(provide 'erc-setup)

;;; erc-setup.el ends here
