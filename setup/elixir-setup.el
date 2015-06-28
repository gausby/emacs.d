(add-to-list 'load-path "~/Projects/emacs-elixir/")
(add-to-list 'load-path "~/Projects/alchemist.el/")

(require 'elixir-mode)
(require 'alchemist)

(eval-after-load 'smartparens
  '(progn
     (defun my-elixir-do-end-close-action (id action context)
       (when (eq action 'insert)
	 (newline-and-indent)
	 (previous-line)
	 (indent-according-to-mode)))

     (sp-with-modes '(elixir-mode)
       (sp-local-pair "do" "end"
		      :when '(("SPC" "RET"))
		      :post-handlers '(:add my-elixir-do-end-close-action)
		      :actions '(insert)))))

(defun t-elixir-mode-hook ()
  (smartparens-mode +1)
  (tester-init-test-run #'alchemist-mix-test-file "_test.exs$")
  (tester-init-test-suite-run #'alchemist-mix-test))

(defun t-erlang-mode-hook ()
  (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))

(add-hook 'elixir-mode-hook 't-elixir-mode-hook)
(add-hook 'erlang-mode-hook 't-erlang-mode-hook)

(provide 'elixir-setup)
