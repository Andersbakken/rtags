;;; ac-rtags.el --- auto-complete back-end for RTags

;; Copyright (C) 2011-2017  Jan Erik Hanssen and Anders Bakken

;; Author: Jan Erik Hanssen <jhanssen@gmail.com>
;;         Anders Bakken <agbakken@gmail.com>
;; URL: http://rtags.net
;; Package-Requires: ((auto-complete "1.4.0") (rtags "2.9"))

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

(require 'auto-complete)
(eval-when-compile (require 'cl))

(defgroup ac-rtags nil
  "Auto completion back-end for RTags."
  :prefix "rtags-"
  :group 'ac
  :group 'rtags
  :link '(url-link :tag "Website" "http://rtags.net"))

(defconst rtags-location-regx "\\([^:]*\\):\\([0-9]*\\):\\([0-9]*\\)")

(defcustom ac-rtags-expand-functions t
  "Whether to expand function parameter lists in `auto-complete' mode."
  :group 'ac-rtags
  :type 'boolean)

(defun ac-rtags-trim-leading-trailing-whitespace (argstr)
  "Remove leading trailing whitespaces from ARGSTR."
  (replace-regexp-in-string
   (rx (one-or-more blank) string-end) ""
   (replace-regexp-in-string (rx string-start (one-or-more blank)) "" argstr)))

(defun ac-rtags-candidates ()
  "Get candidates."
  (let ((buf (current-buffer))
        (loc (rtags-current-location)))
    (when (buffer-file-name buf)
      (with-temp-buffer
        (rtags-call-rc :path (buffer-file-name buf)
                       :unsaved (and (buffer-modified-p buf) buf)
                       "--code-complete-at" loc "--synchronous-completions" "--elisp")
        (goto-char (point-min))
        (when (looking-at "(")
          (let ((data
                 (condition-case nil
                     (eval (read (current-buffer)))
                   (error
                    (message "****** Got Completion Error ******")
                    nil))))
            (and (eq (car data) 'completions)
                 (mapcar #'(lambda (elem)
                             (propertize (car elem)
                                         'ac-rtags-full (cadr elem)
                                         'ac-rtags-type (caddr elem)))
                         (cadadr data)))))))))

(defun ac-rtags-document (item)
  "Get property text from ITEM."
  (get-text-property 0 'ac-rtags-full item))

(defun ac-rtags-action ()
  ;; propertized string of last completion is cdr of `ac-last-completion'
  (let* ((last-compl (cdr ac-last-completion))
         (type (get-text-property 0 'ac-rtags-type last-compl))
         (tag (ac-rtags-document last-compl)))
    (cond ((or (string= type "CXXMethod")
               (string= type "FunctionDecl")
               (string= type "FunctionTemplate"))
           (and ac-rtags-expand-functions (ac-rtags-action-function tag)))
          ((or (string= type "Namespace")
               (string= type "NamespaceAlias"))
           (ac-rtags-action-namespace tag))
          (t
           nil))))

(defun ac-rtags-action-function (origtag)
  ;; grab only inside the func arg list: int func( int x, int y )
  ;;                                              ^............^
  (let* ((tag (replace-regexp-in-string
               ".*(" "" (replace-regexp-in-string ").*" "" origtag)))
         (arglist (mapcar #'ac-rtags-trim-leading-trailing-whitespace
                          (split-string tag "," t)))
         insertfunc inserttxt)

    ;; for yasnippet, wrap each elem in arg list with ${}
    ;; 'int arg' => ${int arg}
    (cond ((featurep 'yasnippet)
           (setq inserttxt (mapconcat #'(lambda (arg)
                                          (format "%s%s%s" "${" arg "}"))
                                      arglist
                                      ", "))
           (setq insertfunc #'yas-expand-snippet))
          ;; if no yasnippet, just dump the signature
          (t
           (setq insertfunc #'(lambda (txt) (save-excursion (insert txt)) (forward-char)))
           (setq inserttxt (mapconcat 'identity arglist ", "))))
    (apply insertfunc (list (concat "(" inserttxt ")")))))

(defun ac-rtags-action-namespace (origtag)
  (insert "::"))

(defun ac-rtags-prefix ()
  ;; shamelessly borrowed from clang-complete-async
  (or (ac-prefix-symbol)
      (let ((c (char-before)))
        (when (or (eq ?\. c)
                  ;; ->
                  (and (eq ?> c)
                       (eq ?- (char-before (1- (point)))))
                  ;; ::
                  (and (eq ?: c)
                       (eq ?: (char-before (1- (point))))))
          (point)))))

(defun ac-rtags-init ()
  (rtags-diagnostics))

(defun ac-rtags-completions-hook ()
  (ac-start))

(add-hook 'rtags-completions-hook 'ac-rtags-completions-hook)

(ac-define-source rtags
  '((init . ac-rtags-init)
    (prefix . ac-rtags-prefix)
    (candidates . ac-rtags-candidates)
    (action . ac-rtags-action)
    (document . ac-rtags-document)
    (requires . 0)
    (symbol . "r")))

(provide 'ac-rtags)

;;; ac-rtags.el ends here
