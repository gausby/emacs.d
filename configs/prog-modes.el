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
;; elixir specific
;;
(add-to-list 'load-path "~/Development/github.com/gausby/emacs-elixir/")
(add-to-list 'load-path "~/Development/github.com/gausby/alchemist.el/")

(require 'elixir-mode)

(with-eval-after-load 'elixir-mode
  (require 'alchemist)
  ;; Open a new line with a pipe on control return
  (defun mg/open-new-line-with-pipe ()
    "open a new line with a pipe"
    (interactive)
    (progn
      (newline)
      (insert "|> ")
      (indent-according-to-mode)))

  (defun mg/elixir-mode-hook ()
    (alchemist-mode +1)
    ;; (yas/minor-mode +1)
    (flyspell-prog-mode))
  (add-hook 'elixir-mode-hook 'mg/elixir-mode-hook)

  (define-key elixir-mode-map [(control return)] #'mg/open-new-line-with-pipe)
  (define-key elixir-mode-map (kbd "C-c SPC") #'alchemist-mix)
  (define-key elixir-mode-map (kbd "C-c C-c") #'alchemist-mix-compile))

;; scratch pad buffer
(defun mg/alchemist-create-scratch-buffer ()
  "Open a buffer in elixir/alchemist mode; evaluating the
expressions with Elixir"
  (interactive)
  (switch-to-buffer "*elixir scratch*")
  (elixir-mode)
  (alchemist-mode))


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
    (add-hook 'tuareg-mode-hook 'merlin-mode t)))

(with-eval-after-load 'merlin
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
  (setq elm-oracle-command "~/.nvm/versions/node/v5.8.0/bin/elm-oracle"
        elm-compile-command "~/.elmenv/shims/elm-make"
        elm-create-package-command "~/.elmenv/shims/elm-make --yes"
        elm-interactive-command "~/.elmenv/shims/elm-repl"
        elm-package-command "~/.elmenv/shims/elm-package"
        elm-reactor-command "~/.elmenv/shims/elm-reactor"))
(with-eval-after-load 'elm-format
  (setq elm-format-command "~/.local/bin/elm-format-0.18"))


;;
;; Rust
;;
(el-get-bundle rust-mode)
(el-get-bundle flycheck-rust)
(el-get-bundle cargo)
(el-get-bundle rust-racer
  :type github :pkgname "phildawes/racer"
  :description "Rust code completion and code navigation"
  :build '(("cargo" "build" "--release"))
  :prepare (setq racer-cmd (concat emacs-config-dir "el-get/rust-racer/target/release/racer")
                 racer-rust-src-path "~/.multirust/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")
  :post-init (add-hook 'racer-mode-hook #'eldoc-mode))

(el-get-bundle emacs-racer
  :type github :pkgname "racer-rust/emacs-racer"
  :description "Racer support for Emacs"
  :depends (rust-mode company-mode dash s f)
  :prepare (add-hook 'rust-mode-hook #'racer-mode))
