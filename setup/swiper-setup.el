;;; swiper-setup.el --- setup for swiper

;;; Commentary:
;;

;;; Code:
(require 'swiper)

(ivy-mode 1)

;; don't show recent closed items in various buffers
(setq ivy-use-virtual-buffers nil)

;; keybindings
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)

(provide 'swiper-setup)

;;; swiper-setup.el ends here
