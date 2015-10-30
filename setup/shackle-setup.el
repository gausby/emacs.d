;;; shackle-setup.el --- setup for shackle

;;; Commentary:
;;  Setup for how certain buffers behave

;;; Code:
(require 'shackle)

(shackle-mode)
(custom-set-variables
 '(shackle-rules
   '(("*Ido Completions*" :align below :ratio 0.33)
     ("*Help*" :align below :ratio 0.33 :select t)
     ("*Buffer List*" :align below :ratio 0.33 :select t)

     ("*alchemist help*" :align below :ratio 0.75 :select t)
     ("*alchemist test report*" :align right :ratio 0.45 :select t)
     ("*Alchemist-IEx*" :align right :ratio 0.45 :select t)

     ("*ag search*" :align below :ratio 0.33 :select t))))

(provide 'shackle-setup)
;;; shackle-setup.el ends here
