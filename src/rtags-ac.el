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

(require 'auto-complete)
(eval-when-compile (require 'cl))

(defgroup rtags-ac nil
  "Auto completion back-end for RTags."
  :prefix "rtags-"
  :group 'ac
  :group 'rtags
  :link '(url-link :tag "Website" "http://rtags.net"))

(defconst rtags-location-regx "\\([^:]*\\):\\([0-9]*\\):\\([0-9]*\\)")

(defcustom rtags-ac-expand-functions t
  "Whether to expand function parameter lists in `auto-complete' mode."
  :group 'rtags-ac
  :type 'boolean)

(defun rtags-ac-trim-leading-trailing-whitespace (argstr)
  (replace-regexp-in-string
   (rx (one-or-more blank) string-end) ""
   (replace-regexp-in-string (rx string-start (one-or-more blank)) "" argstr)))

(defun rtags-ac-candidates ()
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
                                         'rtags-ac-full (cadr elem)
                                         'rtags-ac-type (caddr elem)))
                         (cadadr data)))))))))

(defun rtags-ac-document (item)
  (get-text-property 0 'rtags-ac-full item))

(defun rtags-ac-action ()
  ;; propertized string of last completion is cdr of `ac-last-completion'
  (let* ((last-compl (cdr ac-last-completion))
         (type (get-text-property 0 'rtags-ac-type last-compl))
         (tag (rtags-ac-document last-compl)))
    (cond ((or (string= type "CXXMethod")
               (string= type "FunctionDecl")
               (string= type "FunctionTemplate"))
           (and rtags-ac-expand-functions (rtags-ac-action-function tag)))
          ((or (string= type "Namespace")
               (string= type "NamespaceAlias"))
           (rtags-ac-action-namespace tag))
          (t
           nil))))

(defun rtags-ac-action-function (origtag)
  ;; grab only inside the func arg list: int func( int x, int y )
  ;;                                              ^............^
  (let* ((tag (replace-regexp-in-string
               ".*(" "" (replace-regexp-in-string ").*" "" origtag)))
         (arglist (mapcar #'rtags-ac-trim-leading-trailing-whitespace
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

(defun rtags-ac-action-namespace (origtag)
  (insert "::"))

(defun rtags-ac-prefix ()
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

(defun rtags-ac-init ()
  (rtags-diagnostics))

(defun rtags-ac-completions-hook ()
  (ac-start))

(add-hook 'rtags-completions-hook 'rtags-ac-completions-hook)

(ac-define-source rtags
  '((init . rtags-ac-init)
    (prefix . rtags-ac-prefix)
    (candidates . rtags-ac-candidates)
    (action . rtags-ac-action)
    (document . rtags-ac-document)
    (requires . 0)
    (symbol . "r")))

(provide 'rtags-ac)

;;; rtags-ac.el ends here
