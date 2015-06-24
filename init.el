(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize)

(add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))

(require 'system-setup)

(require 'company-setup)
(require 'elixir-setup)
(require 'emacs-setup)
(require 'god-mode-setup)
(require 'looks-setup)
(require 'popwin-setup)

(setq custom-file (expand-file-name "customize.el" user-emacs-directory))
(load custom-file)

(provide 'init)
