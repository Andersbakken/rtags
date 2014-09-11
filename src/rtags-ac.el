;; This file is part of RTags.
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

(eval-when-compile (require' cl))
(require 'auto-complete)
(require 'rtags)

(defconst rtags-location-regx
  (rx (group (zero-or-more (not (any ":"))))
      ":"
      (group (zero-or-more digit))
      ":"
      (group (zero-or-more digit))))

(defcustom rtags-ac-expand-functions t
  "Whether to expand function parameter lists in auto-complete mode"
  :group 'rtags
  :type 'boolean)

(defmacro rtags-parse-location (locstr)
  `(when (string-match rtags-location-regx ,locstr)
     (list (match-string 1 ,locstr)
           (match-string 2 ,locstr)
           (match-string 3 ,locstr))))

(defun rtags-ac-trim-leading-trailing-whitespace (argstr)
  (replace-regexp-in-string
   (rx (one-or-more blank) string-end) ""
   (replace-regexp-in-string (rx string-start (one-or-more blank)) "" argstr)))

(defun rtags-ac-candidates ()
  ;; locstr is fullpath_srcfile:row#:col#
  (let* ((locstr (or (and rtags-last-completions
                          (car rtags-last-completions))
                     ""))
         (locinfo (rtags-parse-location locstr))
         (complpt (rtags-calculate-completion-point))
         filefull file row col)

    (when locinfo
      (setq filefull (car locinfo)
            row (caddr locinfo)
            col (cadddr locinfo)
            file (file-name-nondirectory filefull)))

    ;; if last completion was in this src file @ last completion pos
    ;; build a list of completion strings; example format:
    ;; #("word" 'rtags-ac-full "void word(int x)" 'rtags-ac-type "FunctionDecl")
    (if (and (string= (buffer-name (current-buffer)) file)
             complpt
             (cdr-safe rtags-last-completion-position)
             (= complpt (cdr rtags-last-completion-position)))
        (mapcar #'(lambda (elem)
                    (propertize (car elem)
                                'rtags-ac-full (cadr elem)
                                'rtags-ac-type (caddr elem)))
                (cadr rtags-last-completions))
      ;; else forcefully update completions if the compl pos has changed
      ;; checking compl pos helps keep the process buffer from getting slammed
      (rtags-update-completions (not (= (or complpt -1)
                                        (or (cdr-safe rtags-last-completion-position) -1))))
      ;; return nil as `ac-update-greedy' expects us to return a list or nil
      nil)))

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
               (rx (zero-or-more any) "(") ""
               (replace-regexp-in-string (rx ")" (zero-or-more any)) "" origtag)))
         (arglist (mapcar #'rtags-ac-trim-leading-trailing-whitespace
                          (split-string tag
                                        (rx (or ","))
                                        t)))
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
  (unless rtags-diagnostics-process
    (rtags-diagnostics)))

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
