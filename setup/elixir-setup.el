;;; elixir-setup.el --- setup for elixir programming environment

;;; Commentary:
;; Notice that this setup needs alchemist and elixir-mode to be checked out in ~/Development/forks.
;; I keep a fork of the two projects on github, I got upstream set to the official repos and sync
;; them regularly.
;;
;;   * enable elixir- and alchemist-mode
;;   * setup my yasnippets, these are found in `~/.emacs.d/snippets/elixir-mode`
;;   * highlight matching do..end-pairs
;;   * set `M-,` to jump-back to code when visiting definition in Erlang code

;;; Code:
(add-to-list 'load-path "~/Development/forks/emacs-elixir/")
(add-to-list 'load-path "~/Development/forks/alchemist.el/")

(require 'elixir-mode)
(require 'alchemist)

(defun mg/elixir-mode-hook ()
  (alchemist-mode +1)
  (yas/minor-mode +1)
  (smartparens-mode +1))

(defun mg/erlang-mode-hook ()
  (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))

(add-hook 'elixir-mode-hook 'mg/elixir-mode-hook)
(add-hook 'erlang-mode-hook 'mg/erlang-mode-hook)

(defun mg/alchemist-iex-mode-hook ()
  (text-scale-set -3))
(add-hook 'alchemist-iex-mode-hook 'mg/alchemist-iex-mode-hook)

(provide 'elixir-setup)

;;; elixir-setup.el ends here
