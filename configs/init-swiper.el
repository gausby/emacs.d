(ivy-mode 1)

;; don't show recent closed items in various buffers
(setq ivy-use-virtual-buffers nil)


;; Advices -------------------------------------------------------------

;; fix position when exiting swiper
;; taken from: http://pragmaticemacs.com/emacs/dont-search-swipe/
(defun mg/swiper-recenter (&rest args)
  "recenter display after swiper"
  (recenter))
(advice-add 'swiper :after #'mg/swiper-recenter)


;; Keybindings ---------------------------------------------------------
(global-set-key (kbd "M-x") 'counsel-M-x)
