;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
; (package-initialize)

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))

(require 'helpers)
(require 'system-setup)

(require 'browser-setup)
(require 'bbdb-setup)
(require 'company-setup)
(require 'diary-mode-setup)
(require 'elixir-setup)
(require 'elm-setup)
(require 'erc-setup)
(require 'emacs-lisp-setup)
(require 'emacs-setup)
(require 'emms-system-setup)
(require 'expand-region-setup)
(require 'git-setup)
(require 'god-mode-setup)
(require 'haskell-setup)
(require 'irfc-setup)
(require 'julia-setup)
(require 'latex-setup)
(require 'looks-setup)
(require 'mail-setup)
(require 'markdown-mode-setup)
(require 'navigation-setup)
(require 'notifications-setup)
(require 'org-setup)
(require 'project-setup)
(require 'shackle-setup)
(require 'smex-setup)
(require 'webdev-setup)
(require 'yasnippet-setup)

(setq custom-file (expand-file-name "customize.el" user-emacs-directory))
(load custom-file)

(provide 'init)
