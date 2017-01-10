(el-get-bundle elpa:material-theme
  (progn
    (setq custom-safe-themes t)
    (load-theme 'material)))

;; get rid of the start up screen
(setq inhibit-startup-message t)


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

(setq uniquify-buffer-name-style 'forward)

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

    ;; disable osx native fullscreen and toggle it like most other OS X programs
    (setq ns-use-native-fullscreen nil)
    (global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
))
