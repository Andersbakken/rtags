;;; rtags.el --- A front-end for rtags

;; Copyright (C) 2011-2015  Jan Erik Hanssen and Anders Bakken

;; Author: Jan Erik Hanssen <jhanssen@gmail.com>
;;         Anders Bakken <agbakken@gmail.com>
;; URL: http://rtags.net

;; This file is not part of GNU Emacs.

;; This file is part of RTags (http://rtags.net).
;;
;; RTags is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; RTags is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with RTags.  If not, see <http://www.gnu.org/licenses/>.

(require 'company)
(require 'company-template)

(eval-when-compile (require 'rtags))

(defvar company-rtags-modes '(c-mode c++-mode objc-mode)
  "Major modes which rtags may complete.")

(defcustom company-rtags-begin-after-member-access t
  "When non-nil, automatic completion will start whenever the current
symbol is preceded by \".\", \"->\" or \"::\", ignoring
`company-minimum-prefix-length'.

If `company-begin-commands' is a list, it should include `c-electric-lt-gt'
and `c-electric-colon', for automatic completion right after \">\" and
\":\"."
  :group 'rtags
  :type 'boolean)

(defcustom company-rtags-max-wait 100
  "Max number of waits company-rtags will do before giving up (max wait time is (* company-rtags-max-wait company-async-wait))"
  :group 'rtags
  :type 'integer)

(defcustom company-rtags-use-async t
  "Whether to use async completions for company-rtags"
  :group 'rtags
  :type 'boolean)

(defcustom company-rtags-insert-arguments t
  "When non-nil, insert function arguments as a template after completion."
  :group 'rtags
  :type 'boolean)

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

(defun company-rtags--prefix-type ()
  (let ((symbol (company-grab-symbol)))
    (when symbol
      (save-excursion
        (forward-char (- (length symbol)))
        (cond ((looking-back "\\." (1- (point))) 'company-rtags-dot)
              ((looking-back "\\->" (- (point) 2)) 'company-rtags-arrow)
              ((looking-back "\\::" (- (point) 2)) 'company-rtags-colons)
              (t nil))))))

(defun company-rtags--valid-candidate (prefix cand)
  (and (> (length (car cand)) 0)
       (or (not prefix)
           (string-prefix-p prefix (car cand)))
       (not (string= (nth 2 cand) "NotImplemented"))
       (let ((prefix-type (company-rtags--prefix-type)))
         (or (not prefix-type)
             (eq prefix-type 'company-rtags-colons)
             (not (string= (nth 2 cand) "EnumConstantDecl"))))))

(defun company-rtags--make-candidate (candidate maxwidth)
  (let* ((text (copy-sequence (nth 0 candidate)))
         (meta (nth 1 candidate))
         (metalength (length meta)))
    (put-text-property 0 1 'meta-insert meta text)
    (when (> metalength maxwidth)
      (setq meta (concat (substring meta 0 (- maxwidth 5)) "<...>)")))
    (put-text-property 0 1 'meta meta text)
    text))

(defun company-rtags--candidates (prefix)
  (when (rtags-has-diagnostics)
    (let ((updated (rtags-update-completions)))
      (when updated
        (if (numberp updated)
            (let ((old rtags-last-completions)
                  (maxwait company-rtags-max-wait))
              (while (and (eq old rtags-last-completions)
                          (> maxwait 0))
                (decf maxwait)
                (sleep-for company-async-wait))))
        (if (and rtags-last-completions
                 (eq (current-buffer) (car rtags-last-completion-position))
                 (= (or (rtags-calculate-completion-point) -1) (cdr rtags-last-completion-position)))
            (let (results
                  (candidates (cadr rtags-last-completions))
                  (maxwidth (- (window-width) (- (point) (point-at-bol)))))
              (while candidates
                (when (company-rtags--valid-candidate prefix (car candidates))
                  (push (company-rtags--make-candidate (car candidates) maxwidth) results))
                (setq candidates (cdr candidates)))
              (reverse results)))))))

(defun company-rtags--meta (candidate insert)
  (get-text-property 0 (if insert 'meta-insert 'meta) candidate))

(defun company-rtags--annotation (candidate insert)
  (let ((meta (company-rtags--meta candidate insert)))
    (cond
     ((null meta) nil)
     ((string-match "\\((.*)\\)" meta)
      (match-string 1 meta)))))

(defvar rtags-company-last-completion-position nil)
(defvar rtags-company-last-completion-callback nil)
(defvar rtags-company-last-completion-prefix nil)
(defun rtags-company-update-completions (cb)
  ;; (setq rtags-company-last-completion-prefix prefix)
  (setq rtags-company-last-completion-callback cb)
  (rtags-update-completions)
  (setq rtags-company-last-completion-position rtags-last-completion-position)
  (rtags-company-diagnostics-hook))

(defun rtags-company-diagnostics-hook ()
  (when (and rtags-company-last-completion-callback
             rtags-last-completion-position
             rtags-company-last-completion-position
             (eq (car rtags-last-completion-position) (car rtags-company-last-completion-position))
             (= (cdr rtags-last-completion-position) (cdr rtags-company-last-completion-position)))
    (let ((results nil)
          (maxwidth (max 10 (- (window-width) (- (point) (point-at-bol)))))
          (candidates (cadr rtags-last-completions)))
      (while candidates
        (when (company-rtags--valid-candidate rtags-company-last-completion-prefix (car candidates))
          (push (company-rtags--make-candidate (car candidates) maxwidth) results))
        (setq candidates (cdr candidates)))
      (funcall rtags-company-last-completion-callback (reverse results)))))
(add-hook 'rtags-diagnostics-hook 'rtags-company-diagnostics-hook)

(defun company-rtags (command &optional arg &rest ignored)
  "`company-mode' completion back-end for `rtags'."
  (interactive (list 'interactive))
  (setq rtags-company-last-completion-prefix arg)
  (case command
    (init (or rtags-autostart-diagnostics (rtags-diagnostics)))
    (interactive (company-begin-backend 'company-rtags))
    (prefix (and (memq major-mode company-rtags-modes)
                 buffer-file-name
                 (not (company-in-string-or-comment))
                 (rtags-is-indexed)
                 (company-rtags--prefix)))
    (candidates
     (if company-rtags-use-async
         (cons :async
               (lambda (cb)
                 (rtags-company-update-completions cb)))
       (company-rtags--candidates arg)))
    (meta (company-rtags--meta arg nil))
    (sorted t)
    (annotation (company-rtags--annotation arg nil))
    (post-completion (let ((anno (company-rtags--annotation arg t)))
                       (when (and company-rtags-insert-arguments anno)
                         (insert anno)
                         (company-template-c-like-templatify anno))))))

(provide 'company-rtags)
