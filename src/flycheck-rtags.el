;;; flycheck-rtags.el --- RTags Flycheck integration.

;; Copyright (C) 2017 Christian Schwarzgruber

;; Author: Christian Schwarzgruber <c.schwarzgruber.cs@gmail.com>
;; URL: http://rtags.net
;; Version: 0.2
;; Package-Requires: ((emacs "24") (flycheck "0.23") (rtags "2.9"))

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
;; C, C++ and Objective-c support for Flycheck, using rtags.
;;

;; Usage:
;;
;; (require 'flycheck-rtags)
;;
;;
;; ;; Optional explicitly select the RTags Flycheck checker for c or c++ major mode.
;; ;; Turn off Flycheck highlighting, use the RTags one.
;; ;; Turn off automatic Flycheck syntax checking rtags does this manually.
;; (defun my-flycheck-rtags-setup ()
;;   "Configure flycheck-rtags for better experience."
;;   (flycheck-select-checker 'rtags)
;;   (setq-local flycheck-check-syntax-automatically nil)
;;   (setq-local flycheck-highlighting-mode nil))
;; ;; c-mode-common-hook is also called by c++-mode
;; (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)
;;

;;; Code:

(require 'rtags)

(require 'flycheck)
(eval-when-compile (require 'pcase))

(defgroup flycheck-rtags nil
  "RTags Flycheck integration."
  :prefix "flycheck-"
  :group 'flycheck
  :group 'rtags
  :link '(url-link :tag "Website" "http://rtags.net"))

;; Shamelessly stolen from flycheck-irony
(defcustom flycheck-rtags-error-filter 'identity
   "A function to filter the errors returned by this checker.

See ':error-filter' description in `flycheck-define-generic-checker'.
For an example, take a look at `flycheck-dequalify-error-ids'."
   :type 'function
   :group 'flycheck-rtags)

(defun flycheck-rtags--build-error (checker buffer)
  "Flycheck RTags build error function.
CHECKER is the syntax checker used to parse BUFFER."
  (let* ((diagnostics-buffer (get-buffer rtags-diagnostics-buffer-name))
         (file-name (file-truename (buffer-file-name buffer)))
         (rx (concat "^\\(" file-name "\\):\\([0-9]+\\):\\([0-9]+\\): \\(\\w+\\): \\(.*\\)$"))
         flycheck-errors)
    (with-current-buffer diagnostics-buffer
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp rx nil t)
          (let ((line (string-to-number (match-string-no-properties 2)))
                (column (string-to-number (match-string-no-properties 3)))
                (severity (match-string-no-properties 4))
                (text (match-string-no-properties 5)))
            (when (member severity '("warning" "error" "fixit"))
              (push (flycheck-error-new-at line
                                           column
                                           (pcase severity
                                             ((or `"fixit" `"warning") 'warning)
                                             ((or `"error" `"fatal") 'error))
                                           text
                                           :checker checker
                                           :buffer buffer
                                           :filename file-name)
                    flycheck-errors))))))
    flycheck-errors))

(defun flycheck-rtags--start (checker callback)
  "Flycheck RTags start function.
CHECKER is the syntax checker (RTags).
CALLBACK is the callback function to call."
  (let ((buffer (current-buffer)))
    (rtags-diagnostics)
    (funcall callback 'finished (flycheck-rtags--build-error checker buffer))))

(defun flycheck-rtags--verify (checker)
  "Verify the Flycheck RTags syntax CHECKER."
  (list
   (flycheck-verification-result-new
    :label "RTags enabled"
    :message (if rtags-enabled "enabled" "disabled")
    :face (if rtags-enabled 'success '(bold warning)))))

(flycheck-define-generic-checker 'rtags
  "RTags flycheck checker."
  :start 'flycheck-rtags--start
  :verify 'flycheck-rtags--verify
  :modes rtags-supported-major-modes
  :error-filter flycheck-rtags-error-filter)

(add-to-list 'flycheck-checkers 'rtags)

(provide 'flycheck-rtags)

;;; flycheck-rtags.el ends here
