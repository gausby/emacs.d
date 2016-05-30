;;; window-setup.el --- setup for window handling

;;; Commentary:
;;  Setup for how windows behave
;;
;;  - Shackle will position certain buffers
;;  - winner-mode is enabled

;;; Code:
(require 'shackle)

(shackle-mode)
(custom-set-variables
 '(shackle-rules
   '(("*Ido Completions*" :align below :ratio 0.33)
     ("*Help*" :align below :ratio 0.33 :select t)
     ("*Buffer List*" :align below :ratio 0.33 :select t)

     ("*alchemist help*" :align below :ratio 0.75 :select t)
     ("*alchemist test report*" :align below :ratio 0.5 :select t)
     ("*Alchemist-IEx*" :align right :ratio 0.45 :select t)

     ("*ag search*" :align below :ratio 0.33 :select t))))

;; enable winner mode
(winner-mode t)

(provide 'window-setup)
;;; window-setup.el ends here
