;;; swiper-setup.el --- setup for swiper

;;; Commentary:
;;  * use ivy for completion and complleation
;;  * activate ivy mode for projectile

;;; Code:
(require 'swiper)
(require 'projectile)

(ivy-mode 1)

;; don't show recent closed items in various buffers
(setq ivy-use-virtual-buffers nil)

(setq projectile-completion-system 'ivy)

;; keybindings
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)

(provide 'swiper-setup)

;;; swiper-setup.el ends here
