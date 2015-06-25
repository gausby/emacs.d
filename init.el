(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize)

(add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))

(require 'system-setup)

(require 'ace-setup)
(require 'company-setup)
(require 'elixir-setup)
(require 'elm-setup)
(require 'emacs-setup)
(require 'expand-region-setup)
(require 'god-mode-setup)
(require 'haskell-setup)
(require 'looks-setup)
(require 'popwin-setup)
(require 'smex-setup)

(setq custom-file (expand-file-name "customize.el" user-emacs-directory))
(load custom-file)

(provide 'init)
