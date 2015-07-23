;;; project-setup.el --- setup for neotree-mode

;;; Commentary:
;;  * Globally enable projectile for switching between projects, files
;;    within projects, etc.
;;  * integrate with ag (the silver searcher) for finding stuff
;;  * Make silver surfer resuse the same buffer for search results
;;  * Highlight search term in silver searcher results
;;  * Set up neotree
;;  * Focus neotree on current project root with `C-c p .`

;;; Code:
(require 'neotree)
(require 'projectile)
(require 'ag)

;; ag
(setq ag-reuse-buffers t
      ag-highlight-search t)

;; Projectile
(projectile-global-mode)

;; Neotree
(global-set-key (kbd "C-c .") 'neotree-toggle)
(define-key global-map (kbd "C-c p .") 'neotree-projectile-action)

(setq neo-banner-message nil
      neo-theme 'arrow
      neo-window-width 20)

(provide 'project-setup)

;;; project-setup.el ends here
