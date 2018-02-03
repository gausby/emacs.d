;; the following is taken from jwiegley's emacs config. It makes a
;; convenient way of looking up passwords from the `.authinfo'-file
(defsubst lookup-password (host user port)
    (require 'auth-source)
    (funcall (plist-get (car (auth-source-search :host host
                                                 :user user
                                                 :type 'netrc
                                                 :port port))
                        :secret)))

;; The following is taken from Bodil Stokke's emacs configuration,
;; which is released under the GNU General Public
;; https://github.com/bodil/ohai-emacs/blob/52bd9774aaee38812dc94d4ace3d45e985dbcc92/modules/ohai-editing.el#L93-L123
(defun set-visual-wrap-column (new-wrap-column &optional buffer)
  "Force visual line wrap at NEW-WRAP-COLUMN in BUFFER (defaults
    to current buffer) by setting the right-hand margin on every
    window that displays BUFFER.  A value of NIL or 0 for
    NEW-WRAP-COLUMN disables this behavior."
  (interactive (list (read-number "New visual wrap column, 0 to disable: " (or visual-wrap-column fill-column 0))))
  (if (and (numberp new-wrap-column)
           (zerop new-wrap-column))
      (setq new-wrap-column nil))
  (with-current-buffer (or buffer (current-buffer))
    (visual-line-mode t)
    (set (make-local-variable 'visual-wrap-column) new-wrap-column)
    (add-hook 'window-configuration-change-hook 'update-visual-wrap-column nil t)
    (let ((windows (get-buffer-window-list)))
      (while windows
        (when (window-live-p (car windows))
          (with-selected-window (car windows)
            (update-visual-wrap-column)))
        (setq windows (cdr windows))))))

(defun update-visual-wrap-column ()
  (if (not visual-wrap-column)
      (set-window-margins nil nil)
    (let* ((current-margins (window-margins))
           (right-margin (or (cdr current-margins) 0))
           (current-width (window-width))
           (current-available (+ current-width right-margin)))
      (if (<= current-available visual-wrap-column)
          (set-window-margins nil (car current-margins))
        (set-window-margins nil (car current-margins)
                            (- current-available visual-wrap-column))))))

;; The following code is taken from the Emacs Prelude starter package
;; Prelude is licensed GNU General Public License version 3 and is copy right Bozhidar Batsov
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-A") 'prelude-move-beginning-of-line)
;; https://github.com/bbatsov/prelude/blob/fe7997bc6e05647a935e279094a9c571d175e2dc/core/prelude-core.el#L138-L159

;; the following will make fill-paragraph unfill the paragraph if the
;; command is executed twice in a row
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph] #'endless/fill-or-unfill)
;; taken from http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html


;; Open a new line with a pipe, this will usually be bound to `control
;; return` in programming languages that support a `|>`-pipe
(defun mg/open-new-line-with-pipe ()
  "open a new line with a pipe"
  (interactive)
  (progn
    (newline)
    (insert "|> ")
    (indent-according-to-mode)))


;; The following is very specific to Emacs running in a Darwin
;; environment, it will use the system speech synthesizer to read the
;; selected region out loud; the function `mg/speak-and-insert' can be
;; set as the `flyspell-insert-function' which will speak the inserted
;; word when using `flyspell-auto-correct-word' like so:
;;
;; (setq flyspell-insert-function (function mg/speak-and-insert))
;;
(defcustom mg/speak-voice ""
  "The voice to use, if nil use system voice"
  :type '(choice
          (const :tag "Default" "")
          (const :tag "Magnus" "magnus")
          (const :tag "Thomas" "thomas")
          (const :tag "Samantha" "samantha")))

(defcustom mg/speak-speech-rate 160
  "The rate to use for the speech synthesizer"
  :type 'integer)

(defun mg/speak (word &optional output-buffer)
  (when (eq system-type 'darwin)
    (let ((voice (format "--voice=%s" mg/speak-voice))
          (rate (format "--rate=%d" mg/speak-speech-rate))
          (topic (format "/%s" word)))
      (start-process "speak" output-buffer "say" voice rate topic))))

(defun mg/speak-and-insert (word)
  (progn (mg/speak word)
         (insert word)))

(defun mg/speak-region (start end)
  (interactive "r")
  (let ((topic (format "%s" (buffer-substring-no-properties start end))))
    (mg/speak topic)))
