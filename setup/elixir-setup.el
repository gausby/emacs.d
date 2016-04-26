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
;;   * add an "*elixir scratch* pad buffer" for quick testing and evaluating with `C-c a v q`
;;   * hitting `h` in a test report buffer will activate `highlight-phrase`

;;; Code:
(add-to-list 'load-path "~/Development/forks/emacs-elixir/")
(add-to-list 'load-path "~/Development/forks/alchemist.el/")

(require 'elixir-mode)
(require 'alchemist)
(require 'origami)

(defun mg/elixir-mode-hook ()
  (alchemist-mode +1)
  (yas/minor-mode +1)
  (smartparens-mode +1)
  (origami-mode +1))

(defun mg/erlang-mode-hook ()
  (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))

(add-hook 'elixir-mode-hook 'mg/elixir-mode-hook)
(add-hook 'erlang-mode-hook 'mg/erlang-mode-hook)

(defun mg/alchemist-test-report-mode-hook ()
  (text-scale-set -2))
(add-hook 'alchemist-test-report-mode-hook 'mg/alchemist-test-report-mode-hook)

(define-key alchemist-test-report-mode-map "h" #'highlight-phrase)

(defun mg/alchemist-iex-mode-hook ()
  (text-scale-set -2))
(add-hook 'alchemist-iex-mode-hook 'mg/alchemist-iex-mode-hook)

;; scratch pad buffer
(defun mg/alchemist-create-scratch-buffer ()
  "Open a buffer in elixir/alchemist mode; use `C-c a v q` for
evaluating the expressions in Elixir"
  (interactive)
  (switch-to-buffer "*elixir scratch*")
  (elixir-mode)
  (alchemist-mode))
;; bind the scratch pad to C-c a i s, why 'i'? Dunno, it has something to do with
;; evaluating stuff, I guess.
(define-key alchemist-mode-keymap (kbd "i s") 'mg/alchemist-create-scratch-buffer)

(defun mg/alchemist-run-credo-on-project ()
  "Run credo on project"
  (interactive)
  (alchemist-mix-execute "credo"))
(define-key alchemist-mode-keymap (kbd "p c") 'mg/alchemist-run-credo-on-project)

;; origami
(define-key origami-mode-map (kbd "C-c [") 'origami-close-node)
(define-key origami-mode-map (kbd "C-c ]") 'origami-open-node)

;; require my elixir yasnippets
(require 'mg-elixir-snippets)

(provide 'elixir-setup)

;;; elixir-setup.el ends here
