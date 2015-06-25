;;; smex-setup.el --- setup for smex

;;; Commentary:
;;

;;; Code:
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-auto-merge-work-directories-length -1)

;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
(setq ido-enable-flex-matching t)

(smex-initialize)
(ido-ubiquitous t)

(add-to-list 'ido-ignore-files "\\`node_modules/")

(provide 'smex-setup)

;;; smex-setup.el ends here
