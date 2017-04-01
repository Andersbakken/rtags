;;; helm-rtags.el --- A front-end for rtags

;; Copyright (C) 2011-2015  Jan Erik Hanssen and Anders Bakken

;; Author: Jan Erik Hanssen <jhanssen@gmail.com>
;;         Anders Bakken <agbakken@gmail.com>
;; URL: http://rtags.net
;; Version: 0.2
;; Package-Requires: ((helm "2.0") (rtags "2.9"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'rtags)
(require 'helm)

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
    ret))

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

(defun helm-rtags-transform (candidate)
  "Transform CANDIDATE."
  (let ((line (helm-rtags-get-candidate-line candidate)))
    (when (string-match "\\`\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):\\(.*\\)" line)
      (format "%s:%s:%s:%s"
              (propertize (match-string 1 line) 'face 'helm-rtags-file-face)
              (propertize (match-string 2 line) 'face 'helm-rtags-lineno-face)
              (propertize (match-string 3 line) 'face 'helm-rtags-lineno-face)
              (match-string 4 line)))))

(defvar helm-rtags-source nil)
(setq helm-rtags-source '((name . "RTags Helm")
                          (candidates . helm-rtags-candidates)
                          (real-to-display . helm-rtags-transform)
                          (action . helm-rtags-actions)
                          (persistent-action . helm-rtags-select-persistent)))

(provide 'helm-rtags)

;;; helm-rtags.el ends here
