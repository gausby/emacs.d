;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
; (package-initialize)

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))

(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path site-lisp-dir)
;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(require 'helpers)
(require 'system-setup)

(require 'yasnippet-setup)
(require 'company-setup)
(require 'browser-setup)
(require 'bbdb-setup)
(require 'diary-mode-setup)
(require 'elixir-setup)
(require 'elm-setup)
(require 'erc-setup)
(require 'emacs-lisp-setup)
(require 'emacs-setup)
(require 'spelling-setup)
(require 'emms-system-setup)
(require 'expand-region-setup)
(require 'git-setup)
(require 'god-mode-setup)
(require 'haskell-setup)
(require 'irfc-setup)
(require 'julia-setup)
(require 'latex-setup)
(require 'lfe-setup)
(require 'rust-setup)
(require 'looks-setup)
(require 'mail-setup)
(require 'markdown-mode-setup)
(require 'navigation-setup)
(require 'notifications-setup)
(require 'org-setup)
(require 'project-setup)
(require 'window-setup)
(require 'swiper-setup)
(require 'webdev-setup)

(setq custom-file (expand-file-name "customize.el" user-emacs-directory))
(load custom-file)

(provide 'init)
