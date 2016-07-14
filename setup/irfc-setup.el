;;; irfc-setup.el --- setup for the irfc rfc reader

;;; Commentary:
;;  * Setup for irfc--a RFC reader
;;  * Redefine a ton of the keybindings
;;  * Setup a directory for stroing RFC and files within it should use irfc mode
;;  * Correct some colors for material theme

;;; Code:
(require 'irfc)

(setq irfc-directory "~/Documents/RFC"
      irfc-assoc-mode t)

(add-to-list 'auto-mode-alist
             `(,(format "%s/.*\\.txt" (regexp-quote (expand-file-name irfc-directory))) . irfc-mode))

;; mode hook
(defun mg/irfc-mode-hook ()
  (god-mode-activate 1)
  ;; disable quit buffer, killing it will do fine
  (local-set-key (kbd "q") nil)
  ;; disable custom scroll
  (local-set-key (kbd "e") nil)
  (local-set-key (kbd "<SPC>") nil)
  (local-set-key (kbd "J") nil)
  (local-set-key (kbd "K") nil)

  ;; end and beginning of buffer is normally M-S-> and <
  (local-set-key (kbd ",") nil)
  (local-set-key (kbd ".") nil)


  (local-set-key (kbd "M-p") 'irfc-head-prev)
  (local-set-key (kbd "M-n") 'irfc-head-next)
  (local-set-key (kbd "P") nil)
  (local-set-key (kbd "N") nil)
  (local-set-key (kbd "H") nil)
  (local-set-key (kbd "L") nil)
  (local-set-key (kbd ">") nil)
  (local-set-key (kbd "<") nil))
(add-hook 'irfc-mode-hook 'mg/irfc-mode-hook)

;; font faces
(custom-theme-set-faces 'user
                        ;; structure
                        `(irfc-title-face ((t (:foreground "#eceff1" :weight bold :height 1.2))))

                        ;; table of contents
                        `(irfc-table-item-face ((t (:foreground "#b0bec5" :weight bold))))

                        ;; sections
                        `(irfc-head-name-face ((t (:foreground "#b0bec5" :weight bold))))
                        `(irfc-head-number-face ((t (:foreground "#8a8a8a"))))

                        ;; miscellaneous
                        `(irfc-requirement-keyword-face ((t (:foreground "#f36c60"))))
                        `(irfc-rfc-number-face ((t (:foreground "#fff59d")))))

(provide 'irfc-setup)
;;; irfc-setup.el ends here
