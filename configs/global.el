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
     `(org-level-1 ((t (,@headline :height 1.2))))
     `(org-document-title ((t (,@headline :height 1.25 :underline nil))))
     `(org-block-begin-line ((t (:box nil))))
     `(org-block-end-line ((t (:box nil)))))))


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
          ("*Help*" :select t :align below :size 0.5)
          ("*Flycheck errors*" :select t :align below :size 0.33)
          ("*alchemist test report*" :select t :align below :size 0.5)
          ("*alchemist mix*" :select t :align below :size 0.33))))


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
(el-get-bundle projectile)
(el-get-bundle counsel-projectile
  (global-set-key (kbd "C-c p p") 'counsel-projectile-switch-project)
  (global-set-key (kbd "C-c p f") 'counsel-projectile-find-file)
  (global-set-key (kbd "C-c p b") 'counsel-projectile-switch-to-buffer)
  (global-set-key (kbd "C-c p s") 'counsel-projectile-ag)
  (global-set-key (kbd "C-c p k") 'projectile-kill-buffers)
  (global-set-key (kbd "C-c p t") 'projectile-run-eshell))
(with-eval-after-load 'projectile
  (setq projectile-completion-system 'ivy))


;;
;; Git
;;
(el-get-bundle magit
  (define-key ctl-x-map (kbd "C-g") 'magit-status))
(with-eval-after-load 'magit
  (setq magit-completing-read-function 'ivy-completing-read))

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
