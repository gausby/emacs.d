;;; webdev-setup.el --- setup for web-development related stuff

;;; Commentary:
;;  Setup using web-mode to edit html files and related stuff

;;; Code:
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))

;; CSS
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))

(defun mg/web-mode-hook ()
  "Hooks for web-mode"
  (setq web-mode-indent-style 2
        web-mode-markup-indent-offset 2
        web-mode-enable-current-element-highlight t))

(setq web-mode-ac-sources-alist
  '(("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
    ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
                    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil))
               )))

(add-hook 'web-mode-hook 'mg/web-mode-hook)

(provide 'webdev-setup)

;;; webdev-setup.el ends here
