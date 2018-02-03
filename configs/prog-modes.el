;;
;; Flycheck
;;
(el-get-bundle flycheck
  :post-init
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
    (mg/add-shackle-rule '("*Flycheck errors*" :select t :align below :size 0.33))))


;;
;; Mostly lisp related
;;
(el-get-bundle smartparens
  :post-init
  (with-eval-after-load 'smartparens
    (require 'smartparens-config)
    (show-smartparens-global-mode)))

(el-get-bundle rainbow-delimiters
  :post-init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(el-get-bundle macrostep
  (define-key lisp-interaction-mode-map (kbd "C-c e") 'macrostep-expand)
  (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand))

(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-SPC") #'counsel-imenu)
  (define-key emacs-lisp-mode-map (kbd "M-n") #'sp-beginning-of-next-sexp)
  (define-key emacs-lisp-mode-map (kbd "M-p") #'sp-beginning-of-previous-sexp)
  (add-hook 'emacs-lisp-mode-hook (lambda ()
      (smartparens-strict-mode 1)
      (flyspell-prog-mode))))


;;
;; Elixir specific
;;
(let ((default-directory "~/Development/github.com/gausby/"))
  (add-to-list 'load-path (expand-file-name "emacs-elixir/")))
(autoload 'elixir-mode "elixir-mode" nil t nil)
(add-to-list 'auto-mode-alist '("\\.\\(exs?\\|elixir\\)$" . elixir-mode))
(with-eval-after-load 'elixir-mode
  (let ((default-directory "~/Development/github.com/gausby/"))
    (add-to-list 'load-path (expand-file-name "mg-elixir-snippets/"))
    (add-to-list 'load-path (expand-file-name "alchemist.el/")))
  (require 'mg-elixir-snippets)
  (require 'alchemist)
  ;; Keybindings
  (define-key elixir-mode-map [(control return)] #'mg/open-new-line-with-pipe)
  (define-key elixir-mode-map (kbd "C-c SPC") #'alchemist-mix)
  (define-key elixir-mode-map (kbd "C-c C-c") #'projectile-compile-project)
  (define-key elixir-mode-map (kbd "C-c C-t") #'projectile-test-project)
  (define-key elixir-mode-map (kbd "M-g .") #'alchemist-goto-definition-at-point)
  (define-key elixir-mode-map (kbd "C-c h a") #'alchemist-help)
  (define-key elixir-mode-map (kbd "C-c h .") #'alchemist-help-search-at-point)
  ;; Mode hook
  (add-hook 'elixir-mode-hook (lambda ()
      (alchemist-mode 1)
      (yas/minor-mode 1)
      (smartparens-mode -1)
      ;; compile mode
      (set (make-local-variable 'compilation-read-command) nil)
      (flyspell-prog-mode))))
(with-eval-after-load 'projectile
  (projectile-register-project-type 'elixir '("mix.exs")
      :compile "mix compile"
      :test "mix test"
      :run "mix app.start --temporary"))
(with-eval-after-load 'alchemist
  (let ((default-directory "~/.exenv/shims/"))
    (setq alchemist-mix-command (expand-file-name "mix")
          alchemist-execute-command (expand-file-name "elixir")
          alchemist-compile-command (expand-file-name "elixirc")
          alchemist-iex-program-name (expand-file-name "iex")))
  (dolist ;; shackle rules
      (rule '(("*alchemist test report*" :select t :align below :size 0.5)
              ("*alchemist mix*" :select t :align below :size 0.33)))
    (mg/add-shackle-rule rule)))

;; scratch pad buffer
(defun mg/alchemist-create-scratch-buffer ()
  "Open a buffer in elixir/alchemist mode; evaluating the
expressions with Elixir"
  (interactive)
  (switch-to-buffer "*elixir scratch*")
  (elixir-mode))


;;
;; erlang specific
;;
(el-get-bundle erlang-mode
  :post-init
  (with-eval-after-load 'erlang
    (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back)))


;;
;; Ocaml
;;
;; Will only install the ocaml modes if OPAM is present on the system
;;
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (cond
   ((and opam-share (file-directory-p opam-share))
    (el-get-bundle tuareg-mode
      :post-init
      (progn
        (with-eval-after-load 'tuareg
          (define-key tuareg-mode-map [(control return)] #'mg/open-new-line-with-pipe)
          (define-key tuareg-mode-map (kbd "C-c SPC") #'imenu))
        ;; Add opam emacs directory to the load-path
        (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
        ;; Load merlin-mode
        (autoload 'merlin-mode "merlin" nil t nil)
        ;; Start merlin on ocaml files
        (add-hook 'tuareg-mode-hook #'merlin-mode)))
    (el-get-bundle flyckeck-ocaml
      :type github :pkgname "flycheck/flycheck-ocaml"
      :post-init
      (with-eval-after-load 'merlin
        ;; Setup ocaml flycheck, need to disable merlins own checker first
        (setq merlin-error-after-save nil)
        (flycheck-ocaml-setup)
        ;; Make company aware of merlin
        (add-to-list 'company-backends 'merlin-company-backend)))
    (with-eval-after-load 'org
      (require 'ob-ocaml)
      (add-to-list 'org-babel-load-languages '(ocaml . t))))
   (t (message "Please install OPAM and Merlin"))))


;;
;; Idris
;;
(el-get-bundle idris-mode)


;;
;; Elm
;;
;; elm-format: https://github.com/avh4/elm-format#building-from-source
;;
;;
(el-get-bundle elm-mode
  :post-init
  (progn
    (with-eval-after-load 'elm-mode
      (add-hook 'elm-mode-hook (lambda ()
          (define-key elm-mode-map [(control return)] #'mg/open-new-line-with-pipe)
          (flyspell-prog-mode))))
    (with-eval-after-load 'elm-interactive
      (let ((default-directory "~/.elmenv/shims/"))
        (setq elm-oracle-command "~/.nvm/versions/node/v5.8.0/bin/elm-oracle"
              elm-compile-command (expand-file-name "elm-make")
              elm-create-package-command (expand-file-name "elm-make --yes")
              elm-interactive-command (expand-file-name "elm-repl")
              elm-package-command (expand-file-name "elm-package")
              elm-reactor-command (expand-file-name "elm-reactor"))))
    (with-eval-after-load 'elm-format
      (setq elm-format-command "~/.local/bin/elm-format-0.18"))))


;;
;; Go-lang
;;
(el-get-bundle go-mode
  :post-init
  (add-hook 'go-mode-hook (lambda ()
      (setq gofmt-command "goimports")
      (add-hook 'before-save-hook 'gofmt-before-save)
      ;; keybindings ---------------------------------------------------
      (local-set-key (kbd "C-c C-c") 'compile))))
(el-get-bundle go-eldoc
  :post-init (add-hook 'go-mode-hook 'go-eldoc-setup))
(el-get-bundle go-company
  :post-init
  (add-hook 'go-mode-hook (lambda ()
      (set (make-local-variable 'company-backends) '(company-go)))))
;; todo, ob-go will not evaluate code blocks in org, fix
(el-get-bundle ob-go
  :post-init
  (with-eval-after-load 'org
    (require 'ob-go)
    (add-to-list 'org-babel-load-languages '(go . t))))


;;
;; YAML
;;
(el-get-bundle yaml-mode)


;;
;; Rest client
;;
(el-get-bundle restclient
  :post-init
  (with-eval-after-load 'restclient
    (add-hook 'restclient-mode-hook (lambda ()
      (smartparens-mode 1)
      (flyspell-prog-mode)))
  (mg/add-shackle-rule '("*HTTP Response*" :align below :size 0.3))))
(el-get-bundle company-restclient
  :post-init
  (with-eval-after-load 'restclient
    (add-to-list 'company-backends 'company-restclient)))
;; org-babel support
(el-get-bundle ob-restclient
  :type github :pkgname "alf/ob-restclient.el"
  :description "An extension to restclient.el for emacs that provides org-babel support"
  :depends (restclient)
  :post-init
  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages '(restclient . t))))
