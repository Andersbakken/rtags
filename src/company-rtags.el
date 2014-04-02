(require 'dash)
(require 'company)

(eval-when-compile (require 'rtags))

(defvar company-rtags-modes '(c-mode c++-mode objc-mode)
  "Major modes which rtags may complete.")

(defcustom company-rtags-begin-after-member-access t
  "When non-nil, automatic completion will start whenever the current
symbol is preceded by \".\", \"->\" or \"::\", ignoring
`company-minimum-prefix-length'.

If `company-begin-commands' is a list, it should include `c-electric-lt-gt'
and `c-electric-colon', for automatic completion right after \">\" and
\":\".")

(defun company-rtags--prefix ()
  (let ((symbol (company-grab-symbol)))
    (if symbol
        (if (and company-rtags-begin-after-member-access
                 (save-excursion
                   (forward-char (- (length symbol)))
                   (looking-back "\\.\\|->\\|::" (- (point) 2))))
            (cons symbol t)
          symbol)
      'stop)))

(defun company-rtags--make-candidate (candidate)
  (let ((text (copy-sequence (nth 0 candidate)))
        (meta (nth 1 candidate)))
    (put-text-property 0 1 'meta meta text)
    text))

(defun company-rtags--candidates (prefix)
  (let ((old rtags-last-completions))
    (rtags-update-completions t)
    ;; TODO: fix the potential infinite loop here by checking rdm's availability
    (while (eq old rtags-last-completions)
      (sleep-for company-async-wait))
    (let* ((results (-partition-all-in-steps 3 3 (cadr rtags-last-completions)))
           (relevant-results (--filter (s-starts-with? prefix (car it)) results)))
      (-map 'company-rtags--make-candidate relevant-results))))

(defun company-rtags--meta (candidate)
  (get-text-property 0 'meta candidate))

(defun company-rtags--annotation (candidate)
  (let ((meta (company-rtags--meta candidate)))
    (cond
     ((null meta) nil)
     ((string-match "\\((.*)\\'\\)" meta)
      (match-string 1 meta)))))

(defun company-rtags (command &optional arg &rest ignored)
  "`company-mode' completion back-end for `rtags'."
  (interactive (list 'interactive))
  (case command
    (prefix (and (memq major-mode company-rtags-modes)
                 buffer-file-name
                 (not (company-in-string-or-comment))
                 (company-rtags--prefix)))
    (candidates (company-rtags--candidates arg))
    (meta       (company-rtags--meta arg))
    (annotation (company-rtags--annotation arg))))

(provide 'company-rtags)
