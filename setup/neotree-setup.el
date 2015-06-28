;;; neotree-setup.el --- setup for neotree-mode

;;; Commentary:
;;

;;; Code:
(global-set-key (kbd "C-c .") 'neotree-toggle)

(setq neo-banner-message "Neotree")
(setq neo-theme 'arrow)

(setq neo-window-width 20)

(provide 'neotree-setup)

;;; neotree-setup.el ends here
