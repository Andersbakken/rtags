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

;;; Commentary:

;;; Code:

(require 'rtags)

(require 'company)
(require 'company-template)

(defgroup company-rtags nil
  "Company completion back-end for RTags."
  :prefix "company-"
  :group 'company
  :group 'rtags
  :link '(url-link :tag "Website" "http://rtags.net"))

(defcustom company-rtags-begin-after-member-access t
  "When non-nil, automatic completion will start whenever the current
symbol is preceded by \".\", \"->\" or \"::\", ignoring
`company-minimum-prefix-length'.

If `company-begin-commands' is a list, it should include `c-electric-lt-gt'
and `c-electric-colon', for automatic completion right after \">\" and
\":\"."
  :group 'company-rtags
  :type 'boolean)

(defcustom company-rtags-max-wait 100
  "Max number of waits `company-rtags' will do before giving up.

Maximum wait time is: (* company-rtags-max-wait company-async-wait)"
  :group 'company-rtags
  :type 'integer)

(defcustom company-rtags-use-async t
  "Whether to use async completions for `company-rtags'."
  :group 'company-rtags
  :type 'boolean)

(defcustom company-rtags-insert-arguments t
  "When non-nil, insert function arguments as a template after completion."
  :group 'company-rtags
  :type 'boolean)

(defvar rtags-company-last-completion-location nil)
(defvar rtags-company-last-completion-prefix-type nil)
(defvar rtags-company-last-completion-callback nil)
(defvar rtags-company-last-completion-prefix nil)
(defvar rtags-company-completions-maxwidth nil)

(defun company-rtags--prefix ()
  (let ((symbol (company-grab-symbol)))
    (if symbol
        (if (and company-rtags-begin-after-member-access
                 (save-excursion
                   (forward-char (- (length symbol)))
                   (cond ((looking-back "\\." (1- (point))) 'company-rtags-dot)
                         ((looking-back "\\->" (- (point) 2)) 'company-rtags-arrow)
                         ((looking-back "\\::" (- (point) 2)) 'company-rtags-colons)
                         (t nil))))
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
       (or (not rtags-company-last-completion-prefix-type)
           (eq rtags-company-last-completion-prefix-type 'company-rtags-colons)
           (not (string= (nth 2 cand) "EnumConstantDecl")))))

(defun company-rtags--make-candidate (candidate)
  (let* ((text (copy-sequence (nth 0 candidate)))
         (meta (nth 1 candidate))
         (metalength (length meta)))
    (put-text-property 0 1 'meta-insert meta text)
    (when (> metalength rtags-company-completions-maxwidth)
      ;; (message "text %s meta %s metalength %d max %d"
      ;;          text meta metalength rtags-company-completions-maxwidth)
      (setq meta (concat (substring meta 0 (- rtags-company-completions-maxwidth 5)) "<...>)")))
    (put-text-property 0 1 'meta meta text)
    text))

(defun company-rtags--candidates (prefix)
  (when (rtags-has-diagnostics)
    (let ((updated (rtags-update-completions)))
      (when updated
        (when (numberp updated)
          (let ((old rtags-last-completions)
                (maxwait company-rtags-max-wait))
            (while (and (eq old rtags-last-completions)
                        (> maxwait 0))
              (decf maxwait)
              (sleep-for company-async-wait))))
        (when (and rtags-last-completions
                   (let ((pos (rtags-calculate-completion-point)))
                     (and pos (string= (car rtags-last-completions)
                                       (rtags-current-location pos t)))))
          (let (results
                (candidates (cadr rtags-last-completions)))
            (while candidates
              (when (company-rtags--valid-candidate prefix (car candidates))
                (push (company-rtags--make-candidate (car candidates)) results))
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

(defun rtags-company-completions-calculate-maxwidth ()
  (setq rtags-company-completions-maxwidth (max 10 (- (window-width) (- (rtags-calculate-completion-point) (point-at-bol))))))

(defun rtags-company-update-completions (cb)
  (setq rtags-company-last-completion-callback cb)
  (rtags-update-completions nil rtags-company-last-completion-location)
  (rtags-company-diagnostics-hook))

(defun rtags-company-diagnostics-hook ()
  (when (and rtags-company-last-completion-callback
             rtags-last-completions
             (string= (car rtags-last-completions) rtags-company-last-completion-location))
    (let ((results nil)
          (candidates (cadr rtags-last-completions)))
      (while candidates
        (when (company-rtags--valid-candidate rtags-company-last-completion-prefix (car candidates))
          (push (company-rtags--make-candidate (car candidates)) results))
        (setq candidates (cdr candidates)))
      ;; (message "got candidates %d/%d %s %s %s "
      ;;          (length results)
      ;;          (length (cadr rtags-last-completions))
      ;;          rtags-company-last-completion-prefix
      ;;          (cond ((eq rtags-company-last-completion-prefix-type 'company-rtags-dot) "dot")
      ;;                ((eq rtags-company-last-completion-prefix-type 'company-rtags-colons) "colons")
      ;;                ((eq rtags-company-last-completion-prefix-type 'company-rtags-arrow) "arrow")
      ;;                (t "nil"))
      ;;          (buffer-name))
      (funcall rtags-company-last-completion-callback (reverse results)))))

(add-hook 'rtags-diagnostics-hook 'rtags-company-diagnostics-hook)

(defun company-rtags (command &optional arg &rest ignored)
  "`company-mode' completion back-end for RTags."
  (interactive (list 'interactive))
  (case command
    (init
     (setq rtags-company-last-completion-callback nil)
     (setq rtags-company-last-completion-location nil)
     (or rtags-autostart-diagnostics (rtags-diagnostics)))
    (interactive
     (company-begin-backend 'company-rtags))
    (prefix
     (and (memq major-mode rtags-supported-major-modes)
          buffer-file-name
          (not (company-in-string-or-comment))
          (rtags-is-indexed)
          (company-rtags--prefix)))
    (candidates
     (let ((pos (rtags-calculate-completion-point)))
       (when pos
         (setq rtags-company-last-completion-prefix (if (> (length arg) 0) arg))
         (setq rtags-company-last-completion-prefix-type (company-rtags--prefix-type))
         (setq rtags-company-last-completion-location (rtags-current-location pos t))
         (rtags-company-completions-calculate-maxwidth)
         (if (not company-rtags-use-async)
             (company-rtags--candidates arg)
           (rtags-prepare-completions)
           (cons :async 'rtags-company-update-completions)))))
    (meta
     (company-rtags--meta arg nil))
    (sorted t)
    (annotation
     (company-rtags--annotation arg nil))
    (post-completion
     (let ((anno (company-rtags--annotation arg t)))
       (when (and company-rtags-insert-arguments anno)
         (insert anno)
         (company-template-c-like-templatify anno))))))

(provide 'company-rtags)

;;; company-rtags.el ends here
