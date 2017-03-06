;; get rid of the start up screen
(setq inhibit-startup-message t)


;; theme
(el-get-bundle elpa:material-theme
  (setq custom-safe-themes t)
  (load-theme 'material))
(with-eval-after-load 'material-theme
  ;; Remove the background box on headlines in org-mode buffers
  (let* ((headline `(:background nil :box nil)))
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
     `(org-code ((t (:background "#1e2930")))))))


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

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; overwrite regions on keypress
(delete-selection-mode t)

(global-hl-line-mode t)
(set-default 'indicate-empty-lines t)

;; scroll one line at a time
(setq scroll-step 1
      ;; don't go bananas when scrolling
      scroll-conservatively 10000)

;; Make it difficult to quit emacs
(define-key ctl-x-map (kbd "C-S-c") 'save-buffers-kill-terminal)
(define-key ctl-x-map (kbd "C-c") 'delete-frame)


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
  (progn
    ;; setup copy paste from and to os x
    (defun copy-from-osx ()
      (shell-command-to-string "pbpaste"))
    (defun paste-to-osx (text &optional push)
      (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
    ;; enable the mac keys and fix copy-paste between OSX and Emacs
    (setq interprogram-cut-function 'paste-to-osx
          interprogram-paste-function 'copy-from-osx
          ;; use all the special keys on the mac keyboard
          mac-option-modifier nil
          ns-function-modifier 'super
          mac-right-command-modifier 'hyper
          mac-right-option-modifier 'alt
          mac-command-modifier 'meta)))


;; for max os x systems (only gui) -------------------------------------
(when (eq window-system 'ns)
  (progn
    (setq ns-use-srgb-colorspace t)
    ;; set fonts
    (let ((font "source code pro"))
      (set-frame-font font)
      (set-face-font 'default font)
      (set-face-attribute 'default nil :height 152)
      (set-face-attribute 'mode-line nil :font font :height 100)
      (set-face-attribute 'mode-line-inactive nil :font font :height 100))
    (el-get-bundle 'unicode-fonts (unicode-fonts-setup))
    ;; disable osx native fullscreen and toggle it like most other OS X programs
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


;; projects
(el-get-bundle projectile
  ;; Helpers -----------------------------------------------------------

  ;; a function that will update and prune the projectile project list
  ;; based on my project directory structure
  (defun mg/update-projectile-project-list ()
    (interactive)
    ;; Perform cleanup before adding projects
    (projectile-cleanup-known-projects)
    ;; Iterate over some `project sites', represented as folders in
    ;; ~/Development (i.e. ~/Development/github.com/); within these
    ;; folders each project should be checked out into a folder
    ;; structure of `project-owner/project-name', such as gausby/hazel
    (let* ((default-directory "~/Development")
           (project-sites (list (expand-file-name "github.com")
                                (expand-file-name "gitlab.com"))))
      ;; Filter out nonexistent folders
      (dolist (project-site (seq-filter 'file-exists-p project-sites))
        (dolist (profile-dir (directory-files project-site t "[^\\.]$"))
          (projectile-discover-projects-in-directory profile-dir))))
    ;; Finally, add my Emacs configuration directory
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
  (define-key ctl-x-map (kbd "C-g") 'magit-status))
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
