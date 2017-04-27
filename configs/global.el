;; get rid of the start up screen
(setq inhibit-startup-message t)


;; theme
(el-get-bundle elpa:material-theme
  (setq custom-safe-themes t)
  (load-theme 'material))
(with-eval-after-load 'material-theme
  ;; Remove the background box on headlines in org-mode buffers
  (let ((headline `(:background nil :box nil))
        (class '((class color) (min-colors 89))))
    (custom-theme-set-faces
     'user
     `(org-level-4 ((t (:height 1.0))))
     `(org-level-3 ((t (:height 1.0))))
     `(org-level-2 ((t (,@headline :height 1.0))))
     `(org-level-1 ((t (,@headline :height 1.0))))
     `(org-document-title ((t (,@headline :height 1.25 :underline nil))))
     `(org-block-begin-line ((t (:box nil))))
     `(org-block-end-line ((t (:box nil))))
     `(org-block ((t (:foreground nil :background "#1e2930"))))
     `(org-code ((t (:background "#1e2930"))))
     ;; I find this rainbow-delimiters scheme better for non-lisp modes:
     `(rainbow-delimiters-depth-1-face ((,class (:foreground ,"#C0C0C0"))))
     `(rainbow-delimiters-depth-2-face ((,class (:foreground ,"#B388FF"))))
     `(rainbow-delimiters-depth-3-face ((,class (:foreground ,"#26A69A"))))
     `(rainbow-delimiters-depth-4-face ((,class (:foreground ,"#EF6C00"))))
     `(rainbow-delimiters-depth-5-face ((,class (:foreground ,"#2196F3"))))
     `(rainbow-delimiters-depth-6-face ((,class (:foreground ,"#FFCDD2"))))
     `(rainbow-delimiters-depth-7-face ((,class (:foreground ,"#8BC34A"))))
     `(rainbow-delimiters-depth-8-face ((,class (:foreground ,"#FFF59D"))))
     `(rainbow-delimiters-depth-9-face ((,class (:foreground ,"#795548"))))
     `(rainbow-delimiters-unmatched-face ((,class (:foreground ,"#F0F0F0" :background ,"#F36C60"))))
    )))


;; disable files from being created
(setq create-lockfiles nil
      auto-save-default nil
      ;; disabling version control should speed up things
      vc-handled-backends nil)
;; auto load files when they change on disk
(global-auto-revert-mode t)
;; remove whitespace when buffers are saved
(add-hook 'before-save-hook 'whitespace-cleanup)


;; set text input and behaviour ----------------------------------------
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; overwrite regions on keypress
(delete-selection-mode t)

(global-hl-line-mode t)
(set-default 'indicate-buffer-boundaries '((top . nil) (bottom . right)))

;; scroll one line at a time
(setq scroll-step 1
      ;; don't go bananas when scrolling
      scroll-conservatively 10000)

;; Make it difficult to quit emacs
(define-key ctl-x-map (kbd "C-S-c") 'save-buffers-kill-terminal)
(define-key ctl-x-map (kbd "C-c") 'delete-frame)

;; Remove warnings when using certain commands
(put 'narrow-to-region 'disabled nil)

;; Remap PgUp and PgDn to macro definition and execution. These keys
;; are located far away from the home-row on a Saber68 keyboard.
(global-set-key [(prior)] 'kmacro-start-macro-or-insert-counter)
(global-set-key [(next)] 'kmacro-end-or-call-macro)

;; mode line -----------------------------------------------------------
(setq uniquify-buffer-name-style 'forward)

(el-get-bundle diminish)


;; buffers
(el-get-bundle shackle
  :type github :pkgname "wasamasa/shackle"
  :description "Enforce rules for pop up windows"
  :website "https://github.com/wasamasa/shackle"
  :features shackle
  :prepare (shackle-mode))
(with-eval-after-load 'shackle
  (setq shackle-rules
        '(("*Apropos*" :select t :align below :size 0.5)
          ("*Buffer List*" :select t :align below :size 0.33)
          ("*Help*" :select t :align below :size 0.5))))

(defun mg/add-shackle-rule (rule) (add-to-list 'shackle-rules rule))


;; for all window systems ----------------------------------------------
(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)
  (blink-cursor-mode -1))


;; for max os x systems (gui and terminal) -----------------------------
(when (eq system-type 'darwin)
  ;; use all the special keys on the mac keyboard
  (setq mac-option-modifier nil
        ns-function-modifier 'super
        mac-right-command-modifier 'hyper
        mac-right-option-modifier 'alt
        mac-command-modifier 'meta))


;; for max os x systems (only gui) -------------------------------------
(when (eq window-system 'ns)
  (progn
    (setq ns-use-srgb-colorspace t)
    ;; set fonts
    (let ((font "source code pro"))
      (set-frame-font font)
      (set-face-font 'default font)
      (set-face-attribute 'default nil :height 144)
      (set-face-attribute 'mode-line nil :font font :height 90)
      (set-face-attribute 'mode-line-inactive nil :font font :height 90))
    (el-get-bundle 'unicode-fonts (unicode-fonts-setup))
    ;; disable osx native fullscreen and toggle it like most other OS X apps
    (setq ns-use-native-fullscreen nil)
    (global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)))


;; Calendar and time
(with-eval-after-load 'calendar
  (setq calendar-week-start-day 1
        ;; Location
        calendar-latitude +55.0
        calendar-longitude +12.0
        calendar-location-name "Copenhagen, DK")
  (calendar-set-date-style 'iso))


;; text editing and navigation
(el-get-bundle god-mode)
(with-eval-after-load 'god-mode
  (define-key god-local-mode-map (kbd ".") 'repeat)
  ;; Update the cursor based on the god mode state
  (defun god-mode-update-cursor ()
    (setq cursor-type
          (if (or god-local-mode buffer-read-only) 'hollow 'box)))
  (add-hook 'god-mode-enabled-hook 'god-mode-update-cursor)
  (add-hook 'god-mode-disabled-hook 'god-mode-update-cursor))


(el-get-bundle swiper ;; C-3 «swiper no swiping!»
  :build (("make" "compile")) :info nil ;; (info is broken atm)
  (ivy-mode 1)
  ;; Advices -----------------------------------------------------------
  ;; fix position when exiting swiper
  ;; taken from: http://pragmaticemacs.com/emacs/dont-search-swipe/
  (defun mg/swiper-recenter (&rest args)
    "recenter display after swiper"
    (recenter))
  (advice-add 'swiper :after #'mg/swiper-recenter)
  ;; Keybindings -------------------------------------------------------
  (global-set-key (kbd "M-x") 'counsel-M-x))

(with-eval-after-load 'ivy
  ;; don't show recent closed items in various buffers
  (setq ivy-use-virtual-buffers nil))

(el-get-bundle avy
  (global-set-key (kbd "M-g l") 'avy-goto-line)
  (global-set-key (kbd "M-g SPC") 'avy-goto-char)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1))


(el-get-bundle expand-region
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C-M-=") 'er/contract-region))


;;
;; Compilation mode
;;
(with-eval-after-load 'compile
  ;; fix some problematic buffers with ansi-colors in them--got this
  ;; from http://stackoverflow.com/a/20788581
  (ignore-errors
    (require 'ansi-color)
    (add-hook 'compilation-filter-hook
        (lambda ()
          (when (eq major-mode 'compilation-mode)
            (ansi-color-apply-on-region compilation-filter-start (point-max)))))))


;;
;; projects
;;
(el-get-bundle projectile
  ;; Helpers -----------------------------------------------------------
  (defun mg/update-projectile-project-list ()
  "Discover projects in `~/Development/github.com' and
`~/Development/gitlab.com' and add them to the project list used
by the Projectile project switcher"
  (interactive)
  ;; Perform cleanup before adding projects
  (projectile-cleanup-known-projects)
  ;; Find the projects in the structure and add them
  (let* ((default-directory "~/Development")
         (project-site-globs '("github.com/*/*" "gitlab.com/*/*")))
    ;; The project structure is ~/Development/github.com/USER/PROJECT/
    (dolist (project-site-glob project-site-globs)
      (let ((projects-glob (expand-file-name project-site-glob)))
        (dolist (project (file-expand-wildcards projects-glob))
        (projectile-discover-projects-in-directory project)))))
  ;; Add my Emacs config folder as well ...
  (projectile-discover-projects-in-directory "~/.emacs.d"))
  ;; Run upon initialization
  (mg/update-projectile-project-list)

  ;; Keybindings -------------------------------------------------------
  (define-key ctl-x-map (kbd "p e") 'projectile-edit-dir-locals)
  (define-key ctl-x-map (kbd "p k") 'projectile-kill-buffers)
  (define-key ctl-x-map (kbd "p t") 'projectile-run-eshell))
(el-get-bundle counsel-projectile
  (define-key ctl-x-map (kbd "p p") 'counsel-projectile-switch-project)
  (define-key ctl-x-map (kbd "p f") 'counsel-projectile-find-file)
  (define-key ctl-x-map (kbd "p b") 'counsel-projectile-switch-to-buffer)
  (define-key ctl-x-map (kbd "p s") 'counsel-projectile-ag))

(with-eval-after-load 'projectile
  ;; add directories and files to the projectile ignore list
  (add-to-list 'projectile-globally-ignored-directories "_build")
  (setq projectile-completion-system 'ivy))


;;
;; Git
;;
(el-get-bundle magit
  (define-key ctl-x-map (kbd "g") 'magit-status))
(with-eval-after-load 'magit
  (setq magit-completing-read-function 'ivy-completing-read))
;; can't seem to set this upon `magit-list-repositories' load as its
;; existence is checked before the list is shown
(setq magit-repository-directories '(("~/Development/github.com/" . 2)
                                     ("~/Development/gitlab.com/" . 2)))

(el-get-bundle git)
(el-get-bundle gist)


;;
;; yasnippets
;;
(el-get-bundle yasnippet
  :type github :pkgname "capitaomorte/yasnippet"
  :website "https://github.com/capitaomorte/yasnippet.git"
  :description "YASnippet is a template system for Emacs."
  :compile "yasnippet.el"
  :build nil)
(with-eval-after-load 'yasnippet
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))
(yas-global-mode 1)


;;
;; auto-insert
;;
;; My auto-insert setup has been borrowed from Howard Abrams
;; http://howardism.org/Technical/Emacs/templates-tutorial.html
(setq auto-insert-query nil
      auto-insert-directory (locate-user-emacs-file "templates"))
(add-hook 'find-file-hook 'auto-insert)
(auto-insert-mode 1)

(defun mg/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))


;;
;; Company
;;
(el-get-bundle company-mode
  (global-company-mode 1))
(el-get-bundle company-statistics)
(with-eval-after-load 'company
  (company-statistics-mode 1)
  (setq company-idle-delay 0.3
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t))
