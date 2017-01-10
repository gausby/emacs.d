;; paths ---------------------------------------------------------------
(cond
 ((eq system-type 'darwin)
  (progn
    (push "/usr/local/bin" exec-path)
    (push "/usr/local/sbin" exec-path)
    (push "/usr/bin" exec-path)
    (push "/usr/sbin" exec-path)
    (setenv "PATH"
            (concat "/usr/local/bin:/usr/local/sbin:"
                    "/usr/bin:/usr/sbin:"
                    (getenv "PATH")))
    )))

(setq emacs-config-dir (file-name-directory
                        (or (buffer-file-name) load-file-name)))

(setq custom-file (concat emacs-config-dir "custom.el"))
(defconst *emacs-config-dir* (concat emacs-config-dir "/configs/" ""))

(add-to-list 'load-path (concat emacs-config-dir "/site/"))


;; Write backup files to their own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))


;; package.el configuration --------------------------------------------
(require 'package)
(dolist (arch '(("gnu" . "http://elpa.gnu.org/packages/")
                ("melpa" . "https://melpa.org/packages/")
                ("tromey" . "http://tromey.com/elpa/")
                ))
  (add-to-list 'package-archives arch))
(package-initialize)


;; el-get configuration ------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path (concat emacs-config-dir "/el-get-user/recipes"))

(setq el-get-user-package-directory
      (concat user-emacs-directory "/configs"))


(el-get-bundle swiper
  ;; building swiper with info docs seems to fail at the moment
  :build (("make" "compile")) :info nil
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


;; text editing and navigation
(el-get-bundle expand-region
  (progn
    (global-set-key (kbd "C-=") 'er/expand-region)
    (global-set-key (kbd "C-M-=") 'er/contract-region)))

(el-get-bundle avy
  (progn
    (global-set-key (kbd "M-g l") 'avy-goto-line)
    (global-set-key (kbd "M-g SPC") 'avy-goto-char)
    (global-set-key (kbd "M-g w") 'avy-goto-word-1)))

;; projects
(el-get-bundle counsel-projectile
  (progn
    (global-set-key (kbd "C-c p p") 'counsel-projectile-switch-project)
    (global-set-key (kbd "C-c p f") 'counsel-projectile-find-file)
    (global-set-key (kbd "C-c p b") 'counsel-projectile-switch-to-buffer)
    (global-set-key (kbd "C-c p s") 'counsel-projectile-ag)))

(el-get-bundle magit
  (global-set-key (kbd "C-x g") 'magit-status))

;; prog-modes
(el-get-bundle flycheck)

;; lisp
(el-get-bundle paredit)
(el-get-bundle rainbow-delimiters)


;; A function to load config files
(defun mg/load-config-files (files)
  (dolist (f files)
    (load (expand-file-name (concat *emacs-config-dir* f)))
    (message "Done loading config file: %s" f)))
;; load our config files for the individual modes
(mg/load-config-files
 '("defuns" ;; Has to go first
   "global" ;; Has to go second
   "erc"
   ))
