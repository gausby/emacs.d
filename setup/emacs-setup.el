;;; emacs-setup.el --- editor config

;;; Commentary:
;;  Setup the editor behaviour

;;; Code:

;; disable splash screen
(setq inhibit-startup-message t)

;; should disable .#-files (i actually don't know if this is a good thing)
(setq create-lockfiles nil)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make it difficult to quit emacs
(global-set-key (kbd "C-x C-S-c") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; use all the special keys on the mac keyboard
(setq mac-option-modifier nil)
(setq ns-function-modifier 'super)
(setq mac-right-command-modifier 'hyper)
(setq mac-right-option-modifier 'alt)
(setq mac-command-modifier 'meta)

;; Full screen
;; disable osx native fullscreen
(setq ns-use-native-fullscreen nil)
;; Make full screen work like the most OS X programs
(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)

;; delete selection when active and typing
(delete-selection-mode t)

;; perform whitespace cleanup on save
(add-hook 'before-save-hook 'whitespace-cleanup)

(provide 'emacs-setup)

;;; emacs-setup.el ends here
