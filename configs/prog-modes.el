(el-get-bundle company-mode
  (require 'company)
  (setq company-idle-delay 0.7
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-flip-when-above t)
  (global-company-mode 1))

(el-get-bundle flycheck)

(el-get-bundle paredit
  (progn
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)))

(el-get-bundle rainbow-delimiters
  (progn
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)))

;;
;; elixir specific
;;
(add-to-list 'load-path "~/Development/github.com/gausby/emacs-elixir/")
(add-to-list 'load-path "~/Development/github.com/gausby/alchemist.el/")

(require 'elixir-mode)
(require 'alchemist)

(defun mg/elixir-mode-hook ()
  (alchemist-mode +1)
  ;; (yas/minor-mode +1)
  (flyspell-prog-mode))

;; Open a new line with a pipe on control return
(defun mg/open-new-line-with-pipe ()
     "open a new line with a pipe"
     (interactive)
     (progn
       (newline)
       (insert "|> ")
       (indent-according-to-mode)))
(define-key elixir-mode-map [(control return)] #'mg/open-new-line-with-pipe)

;; scratch pad buffer
(defun mg/alchemist-create-scratch-buffer ()
  "Open a buffer in elixir/alchemist mode; evaluating the
expressions with Elixir"
  (interactive)
  (switch-to-buffer "*elixir scratch*")
  (elixir-mode)
  (alchemist-mode))

(add-hook 'elixir-mode-hook 'mg/elixir-mode-hook)

;;
;; erlang specific
;;
(defun mg/erlang-mode-hook ()
  (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))

;; (el-get-bundle erlang-mode
;;   (progn
;;     (add-hook 'erlang-mode-hook 'mg/erlang-mode-hook)))

;;
;; Ocaml
;;
;; Will only install the ocaml modes if OPAM is present on the sytem
;;
(if (string-equal (substring (shell-command-to-string "which opam 2> /dev/null") 0 -1) "opam not found")
    (message "Please install OPAM and Merlin")
  (el-get-bundle tuareg-mode
    (progn
      ;; Add opam emacs directory to the load-path
      (add-to-list 'load-path
                   (concat (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1)
                           "/emacs/site-lisp"))
      ;; Load merlin-mode
      (autoload 'merlin-mode "merlin" nil t nil)
      ;; Start merlin on ocaml files
      (add-hook 'tuareg-mode-hook 'merlin-mode t)
      ;; Make company aware of merlin
      (with-eval-after-load 'company
        (require 'merlin)
        (add-to-list 'company-backends 'merlin-company-backend)))))


;;
;; Haskell
;;
(el-get-bundle haskell-mode)
(el-get-bundle commercialhaskell/intero
  :depends (haskell-mode flycheck company-mode)
  (progn
    (add-to-list 'load-path (concat emacs-config-dir "el-get/intero/elisp/"))
    (autoload 'intero-mode "intero" nil t nil)
    (add-hook 'haskell-mode-hook 'intero-mode)))


;;
;; Rust
;;
(el-get-bundle rust-mode
  (progn
    (require 'rust-mode)))
(el-get-bundle flycheck-rust)
(el-get-bundle cargo)
(el-get-bundle rust-racer
  :type github
  :pkgname "phildawes/racer"
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
