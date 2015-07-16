;;; looks-setup.el --- editor theme and looks config

;;; Commentary:
;;  Setup the looks of the editor and keys that changes the looks

;;; Code:

;; remove most of the chrome
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; fringes
(set-window-fringes nil 12 0)

;; colors and theme
(setq ns-use-srgb-colorspace t)
(load-theme 'sanityinc-solarized-light t)

;; stop that blinking cursor
(blink-cursor-mode 0)

;; hilight matching parans
(show-paren-mode t)

;; Show empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Scale buffer text size
(global-set-key (kbd "H-+") 'text-scale-increase)
(global-set-key (kbd "H--") 'text-scale-decrease)
(global-set-key (kbd "H-0") 'text-scale-adjust)

;; font and font sizes
(if (equal tty-erase-char 0)
    (progn
      (set-face-attribute 'default nil :font "--source code pro-normal-r-normal-normal-18--128-128-c-*-iso10646-1")
      (set-face-attribute 'mode-line nil :font "--source code pro-normal-r-normal-normal-10--128-128-c-*-iso10646-1")))

(provide 'looks-setup)

;;; looks-setup.el ends here
