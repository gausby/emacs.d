;;; looks-setup.el --- editor theme and looks config

;;; Commentary:
;;  Setup the looks of the editor

;;; Code:

;; remove most of the chrome
(menu-bar-mode -1)
(tool-bar-mode 0)

;; font and font sizes
(if (equal tty-erase-char 0)
    (set-face-attribute 'default nil :font "--source code pro-normal-r-normal-normal-18--128-128-c-*-iso10646-1")
  (set-face-attribute 'mode-line nil :font "--source code pro-normal-r-normal-normal-14--128-128-c-*-iso10646-1")
  )

;; colors and theme
(setq ns-use-srgb-colorspace t)

;; stop that blinking cursor
(blink-cursor-mode 0)

;; highlight the current line
(global-hl-line-mode t)

;; Show empty lines after buffer end
(set-default 'indicate-empty-lines t)

(provide 'looks-setup)

;;; emacs-setup.el ends here
