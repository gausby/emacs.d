(el-get-bundle 'company-mode
  (require 'company)
  (setq company-idle-delay 0.7
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-flip-when-above t)
  (global-company-mode 1))

(el-get-bundle 'flycheck)

(el-get-bundle 'paredit
  (progn
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)))

(el-get-bundle 'rainbow-delimiters
  (progn
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)))

;;
;; elixir specific
;;
(add-to-list 'load-path "~/Development/github.com/gausby/emacs-elixir/")
(add-to-list 'load-path "~/Development/github.com/gausby/alchemist.el/")

(require 'elixir-mode)
(require 'alchemist)

(defun mg/elixir-mode-hook ()
  (alchemist-mode +1)
  ;; (yas/minor-mode +1)
  (flyspell-prog-mode))

;; Open a new line with a pipe on control return
(defun mg/open-new-line-with-pipe ()
     "open a new line with a pipe"
     (interactive)
     (progn
       (newline)
       (insert "|> ")
       (indent-according-to-mode)))
(define-key elixir-mode-map [(control return)] #'mg/open-new-line-with-pipe)

;; scratch pad buffer
(defun mg/alchemist-create-scratch-buffer ()
  "Open a buffer in elixir/alchemist mode; evaluating the
expressions with Elixir"
  (interactive)
  (switch-to-buffer "*elixir scratch*")
  (elixir-mode)
  (alchemist-mode))

(add-hook 'elixir-mode-hook 'mg/elixir-mode-hook)

;;
;; erlang specific
;;
(defun mg/erlang-mode-hook ()
  (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))

(add-hook 'erlang-mode-hook 'mg/erlang-mode-hook)
