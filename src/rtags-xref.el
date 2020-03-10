;;; rtags-xref.el --- RTags backend for xref.el -*- lexical-binding: t -*-

;; Copyright (C) 2019 Jörg Walter

;; Author: Jörg Walter
;; Maintainer: RTags Team
;; URL: https://github.com/Andersbakken/rtags
;; Version: 0.2
;; Package-Requires: ((emacs "25.1") (rtags "2.37"))

;; This file is not part of GNU Emacs.

;; This file is part of RTags (https://github.com/Andersbakken/rtags).
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
;; along with RTags.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This adds support for the Emacs25 xref API (`xref-find-definitions' and
;; friends) to rtags.  Just `require' it and the default Emacs keybindings
;; (M-., M-,, M-? etc.) use rtags.
;;
;; There is just one caveat: `xref-backend-apropos' (`C-M-.' by default) only
;; supports a very limited regex subset: `.' and `.*', plus `^'/`$'.  This is
;; because rtags only supports wildcard searches right now.
;;
;; Apart from that, these bindings expose the full power of rtags.
;;
;; Enable like this:
;;
;;   (require 'rtags-xref)
;;   (add-hook 'c-mode-common-hook #'rtags-xref-enable)
;;

;;; Code:

(require 'xref)
(require 'rtags)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql rtags)))
  "Return the relevant identifier at point."
  (let ((thing (thing-at-point 'symbol)))
    (and thing (propertize (substring-no-properties thing)
                           :rtags-fn (rtags-buffer-file-name)
                           :rtags-loc (rtags-current-location)))))

(defun rtags-xref-backend-find (symbol func)
  "Return definition of SYMBOL, searching via FUNC."
  (save-match-data
    (with-temp-buffer
      (let* ((fn (get-text-property 0 :rtags-fn symbol))
             (loc (get-text-property 0 :rtags-loc symbol))
             (args (if fn (list :path fn "-K") (list "-K"))))
        (apply func symbol loc args))
      (let ((result nil))
        (goto-char (point-min))
        (while (= 0 (forward-line 1))
          (forward-line -1)
          (if (looking-at "\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\)?:\t\\(.*\\)$")
              (let ((file (match-string-no-properties 1))
                    (line (string-to-number (match-string-no-properties 2)))
                    (column (string-to-number (match-string-no-properties 3)))
                    (summary (match-string-no-properties 4)))
                (push (xref-make summary
                                 (xref-make-file-location file line column))
                      result)))
          (forward-line 1))
        (nreverse result)))))

(cl-defmethod xref-backend-definitions ((_backend (eql rtags)) symbol)
  "Return definition of SYMBOL."
  (rtags-xref-backend-find
   symbol
   (lambda (symbol loc &rest args)
     (if loc
         (apply 'rtags-call-rc "-f" loc "--all-targets" args))
     (if (= (rtags-buffer-lines) 0)
         (apply 'rtags-call-rc "-F" symbol "--definition-only" args))
     (if (= (rtags-buffer-lines) 0)
         (apply 'rtags-call-rc "-F" symbol args)))))

(cl-defmethod xref-backend-apropos ((_backend (eql rtags)) pattern)
  "Return symbols that match PATTERN, which only supports `.', `.*', `^', and `$'."
  (if (or (string-match-p "[^.]\\*" pattern)
          (string-match-p "[][\\\\?+{}]" pattern))
      (error "Rtags pattern only supports meta-characters `.', `.*', `^', and `$'"))
  (if (string-match-p "^\\^" pattern)
      (setq pattern (substring pattern 1))
    (setq pattern (concat "*" pattern)))
  (if (string-match-p "\\$$" pattern)
      (setq pattern (substring pattern 0 -1))
    (setq pattern (concat pattern "*")))
  (setq pattern (replace-regexp-in-string "\\.\\*" "*" pattern t t))
  (setq pattern (replace-regexp-in-string "\\." "?" pattern t t))
  (rtags-xref-backend-find
   pattern
   (lambda (symbol _loc &rest args)
     (apply 'rtags-call-rc "-a" "-F" symbol args))))


(cl-defmethod xref-backend-references ((_backend (eql rtags)) symbol)
  "Return references for SYMBOL."
  (rtags-xref-backend-find
   symbol
   (lambda (symbol loc &rest args)
     (if loc
         (apply 'rtags-call-rc "-r" loc args))
     (if (= (rtags-buffer-lines) 0)
         (apply 'rtags-call-rc "-R" symbol args)))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql rtags)))
  "Return completion function."
  #'rtags-symbolname-complete)

(defun rtags-xref-backend () "Return 'rtags." 'rtags)

(defun rtags-xref-enable ()
  "Use rtags as xref backend."
  (interactive)
  (add-hook 'xref-backend-functions #'rtags-xref-backend nil t))

(provide 'rtags-xref)

;;; rtags-xref.el ends here
