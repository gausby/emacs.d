;; paths
(setq emacs-config-dir (file-name-directory
                        (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path (concat emacs-config-dir "/site/"))

(setq custom-file (concat emacs-config-dir "custom.el"))
(defconst *emacs-config-dir* (concat emacs-config-dir "/configs/" ""))

;;; package.el configuration
(require 'package)
(dolist (arch '(("gnu" . "http://elpa.gnu.org/packages/")
                ("melpa" . "https://melpa.org/packages/")
                ("tromey" . "http://tromey.com/elpa/")
                ))
  (add-to-list 'package-archives arch))
(package-initialize)


;; el-get configuration
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(setq el-get-user-package-directory
      (concat user-emacs-directory "/configs"))

;; el-get-sources overrides
(setq el-get-sources
 '((:name material-theme
	  :type elpa)
   ))

(setq my-packages
      (append
       '(el-get)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)

;; A function to load config files
(defun load-config-files (files)
  (dolist (f files)
    (load (expand-file-name
           (concat *emacs-config-dir* f)))
    (message "Loaded config file: %s" f)))
;; load our config files for the individual modes
(load-config-files
 '("defuns" ;; Has to go first
   "global" ;; Has to go second
   ))


(setq custom-safe-themes t)
(load-theme 'material)
