;;; swiper-setup.el --- setup for swiper

;;; Commentary:
;;  * use ivy for completion and complleation
;;  * activate ivy mode for projectile
;;  * Swiper! No swiping!

;;; Code:
(require 'swiper)
(require 'projectile)

(ivy-mode 1)

;; don't show recent closed items in various buffers
(setq ivy-use-virtual-buffers nil)

(setq projectile-completion-system 'ivy)

;; keybindings
(global-set-key (kbd "M-g s") 'swiper)
(global-set-key (kbd "M-g r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)

;; fix position when exiting swiper
;; taken from: http://pragmaticemacs.com/emacs/dont-search-swipe/
(defun mg/swiper-recenter (&rest args)
  "recenter display after swiper"
  (recenter))
(advice-add 'swiper :after #'mg/swiper-recenter)

(provide 'swiper-setup)

;;; swiper-setup.el ends here
