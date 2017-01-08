(require 'erc-view-log)

;; viewing logs ------------------------------------------------------
(setq erc-view-log-my-nickname-match '("gausby"))

(add-to-list 'auto-mode-alist
             `(,(format "%s.*\\.txt" (regexp-quote (expand-file-name erc-log-channels-directory))) . erc-view-log-mode))

(defun mg/erc-view-log-mode-hook ()
  (visual-line-mode)
  (set-visual-wrap-column 80))
(add-hook 'erc-view-log-mode-hook 'mg/erc-view-log-mode-hook)

;; Keybindings ---------------------------------------------------------
(define-key erc-view-log-mode-map (kbd "n") 'next-line)
(define-key erc-view-log-mode-map (kbd "p") 'previous-line)
(define-key erc-view-log-mode-map (kbd "o") 'occur)
(define-key erc-view-log-mode-map (kbd "M-n") 'erc-view-log-next-mention)
(define-key erc-view-log-mode-map (kbd "M-p") 'erc-view-log-previous-mention)
(define-key erc-view-log-mode-map (kbd "q") 'bury-buffer)
