;;; rust-setup.el --- setup for rust-mode

;;; Commentary:
;;  - this setup is based on http://julienblanchard.com/2016/fancy-rust-development-with-emacs/
;;  - Enable cargo mode in rust buffers
;;  - `C-c <tab>` will reformat the buffer, install `cargo install rustfmt`
;;  - Enable flycheck for rust files
;;  - Enable smartparens

;;; Todo:
;;  - look into racer http://julienblanchard.com/2016/fancy-rust-development-with-emacs/#racer

;;; Code:
(require 'rust-mode)
(require 'cargo)

(add-hook 'rust-mode-hook
          (lambda ()
            (progn
              ;; encable cargo in rust buffers
              (cargo-minor-mode 1)
              (smartparens-mode 1)
              ;; reformat buffer
              (local-set-key (kbd "C-c <tab>") #'rust-format-buffer))
            ))


;; flycheck
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(provide 'rust-setup)

;;; rust-setup.el ends here
