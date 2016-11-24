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

(declare-function company-doc-buffer "ext:company")
(declare-function company-manual-begin "ext:company")

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
        (cond ((looking-back "# *include *[<\"]\\([A-Za-z0-9-_./\\]*\\)" (point-at-bol)) (match-string 1))
              ((and company-rtags-begin-after-member-access
                    (not (company-in-string-or-comment))
                    (save-excursion
                      (forward-char (- (length symbol)))
                      (cond ((looking-back "\\." (1- (point))))
                            ((looking-back "\\->" (- (point) 2)))
                            ((looking-back "\\::" (- (point) 2)))
                            (t nil))))
               (cons symbol t))
              (t symbol))
      'stop)))

(defun company-rtags--prefix-type ()
  (let ((symbol (company-grab-symbol))
        (string-or-comment (company-in-string-or-comment)))
    (when symbol
      (save-excursion
        (forward-char (- (length symbol)))
        (cond ((looking-back "# *include *\\([<\"]\\)[A-Za-z0-9-_./\\]*" (point-at-bol)) (if (string= (match-string 1) "\"") 'company-rtags-include-quote : 'company-rtags-include))
              ((and (not string-or-comment) (looking-back "\\." (1- (point)))) 'company-rtags-dot)
              ((and (not string-or-comment) (looking-back "\\->" (- (point) 2))) 'company-rtags-arrow)
              ((and (not string-or-comment) (looking-back "\\::" (- (point) 2))) 'company-rtags-colons)
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
         (brief (nth 3 candidate))
         (metalength (length meta)))
    (put-text-property 0 1 'meta-insert meta text)
    (when (> metalength rtags-company-completions-maxwidth)
      ;; (message "text %s meta %s metalength %d max %d brief %s"
      ;;          text meta metalength rtags-company-completions-maxwidth brief)
      (setq meta (concat (substring meta 0 (- rtags-company-completions-maxwidth 5)) "<...>)")))
    (put-text-property 0 1 'meta meta text)
    (put-text-property 0 1 'brief brief text)
    text))

(defun company-rtags--candidates ()
  (if (member rtags-company-last-completion-prefix-type (list 'company-rtags-include 'company-rtags-include-quote))
      (let* ((file (and (string-match "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\):?[ \t]*\\(.*\\)$" rtags-company-last-completion-location)
                        (match-string 1 rtags-company-last-completion-location)))
             (alternatives (and file
                                (with-temp-buffer
                                  (rtags-call-rc :path file "--code-complete-at" rtags-company-last-completion-location "--code-complete-includes" "--elisp")
                                  (eval (read (buffer-string))))))
             (results))
        (while alternatives
          (let ((text (car alternatives)))
            (when (or (not rtags-company-last-completion-prefix)
                      (string-prefix-p rtags-company-last-completion-prefix text))
              (put-text-property 0 1 'meta-insert (concat text (if (eq rtags-company-last-completion-prefix-type 'company-rtags-include-quote) "\"" ">")) text)
              (push text results))
            (setq alternatives (cdr alternatives))))
        results)
    ;; this needs to call code-complete-at --synchronous-completions
    (let ((buf (current-buffer)))
      (with-temp-buffer
        (rtags-call-rc :path (buffer-file-name buf)
                       :unsaved (and (buffer-modified-p buf) buf)
                       "--code-complete-at" rtags-company-last-completion-location
                       "--synchronous-completions"
                       "--elisp"
                       (if (> (length rtags-company-last-completion-location) 0)
                           (concat "--code-complete-prefix=" rtags-company-last-completion-prefix)))

        (rtags-company--make-candidates)))))

(defun company-rtags--meta (candidate insert)
  (get-text-property 0 (if insert 'meta-insert 'meta) candidate))

(defun company-rtags--doc-buffer (candidate)
  (let ((brief (get-text-property 0 'brief candidate))
        (meta (company-rtags--meta candidate nil)))
    (if meta
        (format "%s\n\n%s" meta brief)
      brief)))

(defun company-rtags--annotation (candidate insert)
  (let ((meta (company-rtags--meta candidate insert)))
    (cond
     ((null meta) nil)
     ((string-match "\\((.*)\\)" meta)
      (match-string 1 meta)))))

(defun rtags-company-completions-calculate-maxwidth ()
  (setq rtags-company-completions-maxwidth (max 10 (- (window-width) (- (rtags-calculate-completion-point) (point-at-bol))))))

(defun rtags-company--make-candidates ()
  (goto-char (point-min))
  (when (looking-at "(")
    (let ((data
           (condition-case nil
               (eval (read (current-buffer)))
             (error
              (message "****** Got Completion Error ******")
              nil))))
      (when (and (eq (car data) 'completions)
                 (string= (rtags-untrampify rtags-company-last-completion-location) (caadr data)))
        (let ((all (cadadr data))
              (completions))
          (while all
            (when (company-rtags--valid-candidate rtags-company-last-completion-prefix (car all))
              (push (company-rtags--make-candidate (car all)) completions))
            (setq all (cdr all)))
          (nreverse completions))))))

(defun rtags-company-code-complete-at-sentinel (process event)
  (let ((status (process-status process)))
    (when (eq status 'exit)
      (with-current-buffer (process-buffer process)
        (let ((completions (rtags-company--make-candidates)))
          (when completions
            (funcall rtags-company-last-completion-callback completions)))))
    (when (memq status '(exit signal closed failed))
      (kill-buffer (process-buffer process)))))

(defun company-rtags (command &optional arg &rest ignored)
  "`company-mode' completion back-end for RTags."
  (interactive (list 'interactive))
  ;; (message "company-rtags %s %s" (symbol-name command) arg)
  (cl-case command
    (init
     (setq rtags-company-last-completion-callback nil)
     (setq rtags-company-last-completion-location nil))
    (interactive
     (company-begin-backend 'company-rtags))
    (prefix
     (and buffer-file-name
          (memq major-mode rtags-supported-major-modes)
          (rtags-is-indexed)
          (company-rtags--prefix)))
    (candidates
     (let ((pos (rtags-calculate-completion-point)))
       (when pos
         (setq rtags-company-last-completion-prefix (if (> (length arg) 0) arg))
         (setq rtags-company-last-completion-prefix-type (company-rtags--prefix-type))
         (setq rtags-company-last-completion-location (rtags-current-location pos t))
         (rtags-company-completions-calculate-maxwidth)
         (company-rtags--candidates))))
    (meta
     (company-rtags--meta arg nil))
    (sorted (not (member rtags-company-last-completion-prefix-type (list 'company-rtags-include 'company-rtags-include-quote))))
    (annotation
     (and (not (member rtags-company-last-completion-prefix-type (list 'company-rtags-include 'company-rtags-include-quote)))
          (company-rtags--annotation arg nil)))
    (doc-buffer
     (company-doc-buffer (company-rtags--doc-buffer arg)))
    (post-completion
     (cond ((eq rtags-company-last-completion-prefix-type 'company-rtags-include)
            (unless (search-forward ">" (point-at-eol) t)
              (insert ">")))
           ((eq rtags-company-last-completion-prefix-type 'company-rtags-include-quote)
            (unless (search-forward "\"" (point-at-eol) t)
              (insert "\"")))
           (t
            (let ((anno (company-rtags--annotation arg t)))
              (when (and company-rtags-insert-arguments anno)
                (insert anno)
                (company-template-c-like-templatify anno))))))))


(defun rtags-completion-at-point ()
  (when (and (company-manual-begin)
             (boundp 'company-common)
             (looking-back company-common (line-beginning-position)))
    (list
     (match-beginning 0)
     (match-end 0)
     (and (boundp 'company-candidates) company-candidates))))
(provide 'company-rtags)

;;; company-rtags.el ends here
