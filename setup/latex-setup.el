;;; latex-setup.el --- setup for latex-mode

;;; Commentary:
;;  * Attempt to get the system to use xetex
;;  * I don't know what is going on here; the documentation on this stuff
;;    is a bit above my head.

;;; Code:

(setq latex-run-command "xelatex")

(setq-default TeX-PDF-mode t)
(setq TeX-engine-alist
      '((pdflatex "pdflatex")
        (xetex "XeTeX shell escape"
               "xetex --file-line-error --shell-escape"
               "xelatex --file-line-error --shell-escape"
               "xetex")))

(setq org-latex-create-formula-image-program 'imagemagick
      org-format-latex-options '(:foreground default
                                 :background default
                                 :scale 3.5
                                 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

(provide 'latex-setup)

;;; latex-setup.el ends here
