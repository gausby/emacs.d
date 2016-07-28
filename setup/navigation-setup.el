;;; navigation-setup.el --- setup for everything navigation

;;; Commentary:
;;  Setup for the various navigation modes
;;
;;  * Use avy to navigate to chars or lines in buffer
;;  * Keyboard prefix for avy is `M-g` because goto-line resides on `M-g g`

;;; Code:
(require 'avy)

(global-set-key (kbd "M-g SPC") 'avy-goto-char)
(global-set-key (kbd "M-g l") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-or-subword-1)

;; bm visual bookmarks
(custom-theme-set-faces 'user
                        `(bm-face ((t (:background "#222C31")))))

(global-set-key (kbd "M-g m") 'bm-toggle)
(global-set-key (kbd "M-g n") 'bm-next)
(global-set-key (kbd "M-g p") 'bm-previous)
(global-set-key (kbd "M-g b") 'bm-show-all)
(global-set-key (kbd "M-g B") 'bm-show)

(provide 'navigation-setup)

;;; navigation-setup.el ends here
