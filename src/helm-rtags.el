;;; helm-rtags.el --- A front-end for rtags -*- lexical-binding: t -*-

;; Copyright (C) 2011-2015  Jan Erik Hanssen and Anders Bakken

;; Author: Jan Erik Hanssen <jhanssen@gmail.com>
;;         Anders Bakken <agbakken@gmail.com>
;; URL: https://github.com/Andersbakken/rtags
;; Version: 0.2
;; Package-Requires: ((helm "2.0") (rtags "2.10"))

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

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'rtags)
(require 'helm)

(defsubst helm-rtags-string-trim-left (string)
  "Remove leading whitespace from STRING."
  (if (string-match "\\`[ \t\n\r]+" string)
      (replace-match "" t t string)
    string))

(defsubst helm-rtags-string-trim-right (string)
  "Remove trailing whitespace from STRING."
  (if (string-match "[ \t\n\r]+\\'" string)
      (replace-match "" t t string)
    string))

(defvar helm-rtags-token nil)

(declare-function helm-highlight-current-line "ext:helm")

(defcustom helm-rtags-actions
  '(("Select" . helm-rtags-select)
    ("Select other window" . helm-rtags-select-other-window))
  "RTags helm actions.
Each element of the alist is a cons-cell of the form (DESCRIPTION . FUNCTION)."
  :group 'rtags
  :type '(alist :key-type string :value-type function))

(defun helm-rtags-candidates ()
  "Get candidates."
  (let ((buf (get-buffer rtags-buffer-name))
        (ret))
    (when buf
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (when (looking-at "Functions called from:")
            (forward-line 1))
          (let (done)
            (while (not done)
              (push (cons (buffer-substring-no-properties (point-at-bol) (point-at-eol)) (point-at-bol)) ret)
              (if (= (point-at-eol) (point-max))
                  (setq done t)
                (forward-line 1)))))))
    (nreverse ret)))

(defun helm-rtags-select (candidate)
  "Select CANDIDATE."
  (with-current-buffer (get-buffer rtags-buffer-name)
    (goto-char candidate)
    (rtags-select nil nil)))

(defun helm-rtags-select-other-window (candidate)
  "Select CANDIDATE in other window."
  (with-current-buffer (get-buffer rtags-buffer-name)
    (goto-char candidate)
    (rtags-select t nil)))

;; (message "CAND: %d" (get-text-property 0 'rtags-buffer-position candidate)))

(defun helm-rtags-get-candidate-line (candidate)
  "Get CANDIDATE line."
  (with-current-buffer (get-buffer rtags-buffer-name)
    (goto-char candidate)
    (buffer-substring-no-properties (save-excursion
                                      (goto-char (point-at-bol))
                                      (skip-chars-forward " ")
                                      (point))
                                    (point-at-eol))))

(defun helm-rtags-select-persistent (candidate)
  "Goto CANDIDATE (Helm persistent action)."
  (let ((line (helm-rtags-get-candidate-line candidate)))
    (rtags-goto-location line t nil)
    (helm-highlight-current-line)))

(defface helm-rtags-file-face
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight file name in the *RTags Helm* buffer."
  :group 'rtags)

(defface helm-rtags-lineno-face
  '((t :inherit font-lock-doc-face))
  "Face used to highlight line number in the *RTags Helm* buffer."
  :group 'rtags)

(defface helm-rtags-token-face
  '((t :inherit font-lock-warning-face
       :background "#212026"))
  "Face used to highlight file name in the *RTags Helm* buffer."
  :group 'rtags)

(defun helm-rtags-transform (candidate)
  "Transform CANDIDATE."
  (let ((line (helm-rtags-get-candidate-line candidate)))
    (when (string-match "\\`\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):\\(.*\\)" line)
      (let* ((file-name (match-string 1 line))
             (line-num (match-string 2 line))
             (column-num (match-string 3 line))
             (content (match-string 4 line))
             (token-begin (string-to-number column-num))
             (token-end (min (+ token-begin (length helm-rtags-token))
                             (length content)))
             (content-prefix (substring content 0 token-begin))
             (content-token (substring content token-begin token-end))
             (content-suffix (substring content token-end (length content))))
        (format "%s:%s:%s: %s%s%s"
                (propertize file-name 'face 'helm-rtags-file-face)
                (propertize line-num 'face 'helm-rtags-lineno-face)
                (propertize column-num 'face 'helm-rtags-lineno-face)
                (helm-rtags-string-trim-left content-prefix)
                (if (string= content-token helm-rtags-token)
                    (propertize content-token 'face 'helm-rtags-token-face)
                  content-token)
                (helm-rtags-string-trim-right content-suffix))))))

(defvar helm-rtags-source nil)
(setq helm-rtags-source '((name . "RTags Helm")
                          (candidates . helm-rtags-candidates)
                          (real-to-display . helm-rtags-transform)
                          (action . helm-rtags-actions)
                          (persistent-action . helm-rtags-select-persistent)))

(defun create-helm-rtags-source (token)
  "Create helm source with TOKEN."
  (setq helm-rtags-token token)
  '(helm-rtags-source))

(provide 'helm-rtags)

;;; helm-rtags.el ends here
