;;; project-setup.el --- setup for neotree-mode

;;; Commentary:
;;  * Globally enable projectile for switching between projects, files
;;    within projects, etc.
;;  * integrate with ag (the silver searcher) for finding stuff
;;  * Make silver surfer resuse the same buffer for search results
;;  * Highlight search term in silver searcher results
;;  * Set up neotree

;;; Code:
(require 'neotree)
(require 'projectile)
(require 'ag)

;; ag
(setq ag-reuse-buffers t
      ag-highlight-search t)

;; Projectile
(projectile-global-mode)

(setq projectile-known-projects-file "~/.projectile-cache/projectile-bookmarks.eld")

;; Neotree
(global-set-key (kbd "C-c .") 'neotree-toggle)

(setq neo-banner-message nil
      neo-theme 'arrow
      neo-window-width 20)

(provide 'project-setup)

;;; project-setup.el ends here
