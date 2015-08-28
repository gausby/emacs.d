;;; emacs-setup.el --- editor config

;;; Commentary:
;;  Setup the editor behaviour

;;; Code:

;; disable splash screen
(setq inhibit-startup-message t)

;; should disable .#-files (i actually don't know if this is a good thing)
(setq create-lockfiles nil)

;; don't create auto save files (those starting with a '#')
(setq auto-save-default nil)

;; utf-8 all the things
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

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

;; setup indentation
(set-default 'indent-tabs-mode nil)
(setq-default tab-width 4)
;; always indent on newlines
(define-key global-map (kbd "RET") 'newline-and-indent)

;; setup scrolling
(setq scroll-step 1)
;; don't go bananas when scrolling
(setq scroll-conservatively 10000)

;; change the way the buffer names are written in the modeline
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; The following code is taken from the Emacs Prelude starter package
;; Prelude is licensed GNU General Public License version 3 and is copy right Bozhidar Batsov
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-A") 'prelude-move-beginning-of-line)
;; https://github.com/bbatsov/prelude/blob/fe7997bc6e05647a935e279094a9c571d175e2dc/core/prelude-core.el#L138-L159

(provide 'emacs-setup)

;;; emacs-setup.el ends here
