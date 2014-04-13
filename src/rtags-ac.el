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

(defmacro rtags-parse-location (locstr)
  `(when (string-match rtags-location-regx ,locstr)
    (list (match-string 1 ,locstr)
	  (match-string 2 ,locstr)
	  (match-string 3 ,locstr))))

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
	     (= complpt (cdr rtags-last-completion-position)))
	(mapcar #'(lambda (elem)
		    (propertize (car elem)
				'rtags-ac-full (cadr elem)
				'rtags-ac-type (caddr elem)))
		(cadr rtags-last-completions))
      ;; else forcefully update completions
      ;; return nil as `ac-update-greedy' expects us to return a list or nil
      (rtags-update-completions t)
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
	   (rtags-ac-action-function tag))
	  (t
	   nil))))

(defun rtags-ac-action-function (tag)
  ;; transform func sig to a list of arg signatures
  (let ((arglist (split-string
		  tag
		  (rx (or "..." ","))
		  t
		  (rx (or (group (zero-or-more any) "(")
			  (group ")" (zero-or-more any))))))
	insertfunc inserttxt)

    ;; for yasnippet, wrap each elem in arg list with ${}
    ;; 'int arg' => ${int arg}
    (cond ((featurep 'yasnippet)
	   (setq inserttxt (mapconcat
			    'identity
			    (mapcar
			     #'(lambda (arg)
				 (when (string-match ".*" arg)
				   (replace-match
				    (concat "${" arg "}")
				    t t arg)))
			     arglist)
			    ", "))
	   (setq insertfunc #'yas-expand-snippet))
	  (t
	   (setq insertfunc #'(lambda (txt) (save-excursion (insert txt)) (forward-char)))
	   (setq inserttxt (mapconcat 'identity arglist ""))))
    (apply insertfunc (list (concat "(" inserttxt ")")))))

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

(ac-define-source rtags
  '((init . rtags-diagnostics)
    (prefix . rtags-ac-prefix)
    (candidates . rtags-ac-candidates)
    (action . rtags-ac-action)
    (document . rtags-ac-document)
    (requires . 0)
    (symbol . "r")))

(provide 'rtags-ac)
