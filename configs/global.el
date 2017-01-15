;; get rid of the start up screen
(setq inhibit-startup-message t)


;; theme
(el-get-bundle elpa:material-theme
  (progn
    (setq custom-safe-themes t)
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
       `(org-block-end-line ((t (:box nil))))))
    (load-theme 'material)))


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
(show-paren-mode t)
(set-default 'indicate-empty-lines t)

;; scroll one line at a time
(setq scroll-step 1)


;; mode line -----------------------------------------------------------
(setq uniquify-buffer-name-style 'forward)

(el-get-bundle diminish)

;; for all window systems ----------------------------------------------
(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)
  (blink-cursor-mode -1))


;; for max os x systems (gui and terminal) -----------------------------
(when (eq system-type 'darwin)
  (progn
    ;; use all the special keys on the mac keyboard
    (setq mac-option-modifier nil)
    (setq ns-function-modifier 'super)
    (setq mac-right-command-modifier 'hyper)
    (setq mac-right-option-modifier 'alt)
    (setq mac-command-modifier 'meta)

    ;; setup copy paste from and to os x
    (defun copy-from-osx ()
      (shell-command-to-string "pbpaste"))
    (defun paste-to-osx (text &optional push)
      (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx)))


;; for max os x systems (only gui) -------------------------------------
(when (eq window-system 'ns)
  (progn
    (setq ns-use-srgb-colorspace t)

    ;; set fonts
    ((lambda (font)
       (set-frame-font font)
       (set-face-attribute 'default nil :font font :height 150 :weight 'normal)
       (set-face-attribute 'mode-line nil :font font :height 100 :weight 'normal)
       (set-face-attribute 'mode-line-inactive nil :font font :height 100 :weight 'normal)
       (set-face-font 'default font))
     "source code pro")
    (el-get-bundle 'unicode-fonts (unicode-fonts-setup))

    ;; disable osx native fullscreen and toggle it like most other OS X programs
    (setq ns-use-native-fullscreen nil)
    (global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
))


;; text editing and navigation
(el-get-bundle swiper ;; C-3 «swiper no swiping!»
  :build (("make" "compile")) :info nil ;; (info is broken atm)
  (progn
    (require 'ivy)
    (ivy-mode 1)
    ;; don't show recent closed items in various buffers
    (setq ivy-use-virtual-buffers nil)

    ;; Advices ---------------------------------------------------------
    ;; fix position when exiting swiper
    ;; taken from: http://pragmaticemacs.com/emacs/dont-search-swipe/
    (defun mg/swiper-recenter (&rest args)
      "recenter display after swiper"
      (recenter))
    (advice-add 'swiper :after #'mg/swiper-recenter)

    ;; Keybindings -----------------------------------------------------
    (global-set-key (kbd "M-x") 'counsel-M-x)
    ))

(el-get-bundle avy
  (progn
    (global-set-key (kbd "M-g l") 'avy-goto-line)
    (global-set-key (kbd "M-g SPC") 'avy-goto-char)
    (global-set-key (kbd "M-g w") 'avy-goto-word-1)))

(el-get-bundle expand-region
  (progn
    (global-set-key (kbd "C-=") 'er/expand-region)
    (global-set-key (kbd "C-M-=") 'er/contract-region)))

;; projects
(el-get-bundle counsel-projectile
  (progn
    (global-set-key (kbd "C-c p p") 'counsel-projectile-switch-project)
    (global-set-key (kbd "C-c p f") 'counsel-projectile-find-file)
    (global-set-key (kbd "C-c p b") 'counsel-projectile-switch-to-buffer)
    (global-set-key (kbd "C-c p s") 'counsel-projectile-ag)))

(el-get-bundle magit
  (global-set-key (kbd "C-x g") 'magit-status))
