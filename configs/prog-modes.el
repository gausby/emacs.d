;;
;; Company
;;
(el-get-bundle company-mode
  (global-company-mode 1))
(with-eval-after-load 'company
  (setq company-idle-delay 0.7
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t))


;;
;; Flycheck
;;
(el-get-bundle flycheck)


;;
;; Mostly lisp related
;;
(el-get-bundle paredit
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(el-get-bundle rainbow-delimiters
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))


;;
;; Elixir specific
;;
(add-to-list 'load-path "~/Development/github.com/gausby/emacs-elixir/")
(autoload 'elixir-mode "elixir-mode" nil t nil)
(add-to-list 'auto-mode-alist '("\\.\\(exs?\\|elixir\\)$" . elixir-mode))
(with-eval-after-load 'elixir-mode
  ;; Yasnippets
  (add-to-list 'load-path "~/Development/github.com/gausby/mg-elixir-snippets/")
  (require 'mg-elixir-snippets)
  ;; Alchemist
  (add-to-list 'load-path "~/Development/github.com/gausby/alchemist.el/")
  (require 'alchemist)
  ;; Keybindings
  (define-key elixir-mode-map [(control return)] #'mg/open-new-line-with-pipe)
  (define-key elixir-mode-map (kbd "C-c SPC") #'alchemist-mix)
  (define-key elixir-mode-map (kbd "C-c C-c") #'alchemist-mix-compile)
  ;; Mode hook
  (add-hook 'elixir-mode-hook (lambda ()
      (alchemist-mode 1)
      (yas/minor-mode 1)
      (flyspell-prog-mode))))

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
  (add-hook 'erlang-mode-hook 'mg/erlang-mode-hook))

(defun mg/erlang-mode-hook ()
  (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))


;;
;; Ocaml
;;
;; Will only install the ocaml modes if OPAM is present on the sytem
;;
(if (string-equal (substring (shell-command-to-string "which opam 2> /dev/null") 0 -1) "opam not found")
    (message "Please install OPAM and Merlin")
  (el-get-bundle tuareg-mode
    ;; Add opam emacs directory to the load-path
    (add-to-list 'load-path
                 (concat (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1)
                         "/emacs/site-lisp"))
    ;; Load merlin-mode
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Start merlin on ocaml files
    (add-hook 'tuareg-mode-hook #'merlin-mode))
  (el-get-bundle flyckeck-ocaml
    :type github :pkgname "flycheck/flycheck-ocaml"))

(with-eval-after-load 'tuareg
  (define-key tuareg-mode-map [(control return)] #'mg/open-new-line-with-pipe)
  (define-key tuareg-mode-map (kbd "C-c SPC") #'imenu))
(with-eval-after-load 'merlin
  ;; Setup ocaml flycheck, need to disable merlins own checker first
  (setq merlin-error-after-save nil)
  (flycheck-ocaml-setup)
  ;; Make company aware of merlin
  (add-to-list 'company-backends 'merlin-company-backend))


;;
;; Haskell
;;
(el-get-bundle haskell-mode)
(el-get-bundle commercialhaskell/intero
  :depends (haskell-mode flycheck company-mode)
  (add-to-list 'load-path (concat emacs-config-dir "el-get/intero/elisp/"))
  (autoload 'intero-mode "intero" nil t nil)
  (add-hook 'haskell-mode-hook 'intero-mode))


;;
;; Elm
;;
;; elm-format: https://github.com/avh4/elm-format#building-from-source
;;
(el-get-bundle elm-mode)
(with-eval-after-load 'elm-interactive
  (let ((default-directory "~/.elmenv/shims/"))
    (setq elm-oracle-command "~/.nvm/versions/node/v5.8.0/bin/elm-oracle"
          elm-compile-command (expand-file-name "elm-make")
          elm-create-package-command (expand-file-name "elm-make --yes")
          elm-interactive-command (expand-file-name "elm-repl")
          elm-package-command (expand-file-name "elm-package")
          elm-reactor-command (expand-file-name "elm-reactor"))))
(with-eval-after-load 'elm-format
  (setq elm-format-command "~/.local/bin/elm-format-0.18"))


;;
;; Rust
;;
(el-get-bundle rust-mode)
(el-get-bundle flycheck-rust)
(el-get-bundle cargo)
(el-get-bundle emacs-racer
  :type github :pkgname "racer-rust/emacs-racer"
  :description "Racer support for Emacs"
  :depends (rust-mode company-mode dash s f)
  :prepare (setq racer-cmd "~/.cargo/bin/racer"
                 racer-rust-src-path "~/.multirust/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")
  :post-init (progn
               (add-hook 'rust-mode-hook #'racer-mode)
               (add-hook 'racer-mode-hook #'eldoc-mode)))
