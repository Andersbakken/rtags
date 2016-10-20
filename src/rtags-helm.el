;;; rtags-helm.el --- A front-end for rtags

;; Copyright (C) 2011-2015  Jan Erik Hanssen and Anders Bakken

;; Author: Jan Erik Hanssen <jhanssen@gmail.com>
;;         Anders Bakken <agbakken@gmail.com>
;; URL: http://rtags.net
;; Version: 2.3.94

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

(defcustom rtags-helm-actions
  '(("Select" . rtags-helm-select)
    ("Select other window" . rtags-helm-select-other-window))
  "RTags helm actions.
Each element of the alist is a cons-cell of the form (DESCRIPTION . FUNCTION)."
  :group 'rtags
  :type '(alist :key-type string :value-type function))

(defun rtags-helm-candidates ()
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

(defun rtags-helm-select (candidate)
  (with-current-buffer (get-buffer rtags-buffer-name)
    (goto-char candidate)
    (rtags-select nil nil)))

(defun rtags-helm-select-other-window (candidate)
  (with-current-buffer (get-buffer rtags-buffer-name)
    (goto-char candidate)
    (rtags-select t nil)))

;; (message "CAND: %d" (get-text-property 0 'rtags-buffer-position candidate)))

(defun rtags-helm-get-candidate-line (candidate)
  (with-current-buffer (get-buffer rtags-buffer-name)
    (goto-char candidate)
    (buffer-substring-no-properties (save-excursion
                                      (goto-char (point-at-bol))
                                      (skip-chars-forward " ")
                                      (point))
                                    (point-at-eol))))

(defun rtags-helm-select-persistent (candidate)
  (let ((line (rtags-helm-get-candidate-line candidate)))
    (rtags-goto-location line t nil)
    (helm-highlight-current-line)))

(defface rtags-helm-file-face
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight file name in the *RTags Helm* buffer."
  :group 'rtags)

(defface rtags-helm-lineno-face
  '((t :inherit font-lock-doc-face))
  "Face used to highlight line number in the *RTags Helm* buffer."
  :group 'rtags)

(defun rtags-helm-transform (candidate)
  (let ((line (rtags-helm-get-candidate-line candidate)))
    (when (string-match "\\`\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):\\(.*\\)" line)
      (format "%s:%s:%s:%s"
              (propertize (match-string 1 line) 'face 'rtags-helm-file-face)
              (propertize (match-string 2 line) 'face 'rtags-helm-lineno-face)
              (propertize (match-string 3 line) 'face 'rtags-helm-lineno-face)
              (match-string 4 line)))))

(defvar rtags-helm-source nil)
(setq rtags-helm-source '((name . "RTags Helm")
                          (candidates . rtags-helm-candidates)
                          (real-to-display . rtags-helm-transform)
                          (action . rtags-helm-actions)
                          (persistent-action . rtags-helm-select-persistent)))

(provide 'rtags-helm)
