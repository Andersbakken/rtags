;;; ivy-rtags.el --- RTags completion back-end for ivy

;; Copyright (C) 2011-2017  Jan Erik Hanssen and Anders Bakken

;; Author: Jan Erik Hanssen <jhanssen@gmail.com>
;;         Anders Bakken <agbakken@gmail.com>
;; URL: http://rtags.net
;; Version: 0.1
;; Package-Requires: ((ivy "0.7.0") (rtags "2.9"))

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
;; Ivy integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'rtags)
(require 'ivy)

(defun ivy-rtags-collection ()
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

(defun ivy-rtags-select (candidate)
  "Select CANDIDATE."
  (with-current-buffer (get-buffer rtags-buffer-name)
    (goto-char (cdr candidate))
    (rtags-select nil nil)))

;; (defun ivy-rtags-select-other-window (candidate)
;;   (with-current-buffer (get-buffer rtags-buffer-name)
;;     (goto-char (cdr candidate))
;;     (rtags-select t nil)))

(defun ivy-rtags-read ()
  "RTags completing read function for `ivy'."
  (ivy-read "RTags Ivy: " (ivy-rtags-collection)
            :require-match t
            :action #'ivy-rtags-select))

(provide 'ivy-rtags)

;;; ivy-rtags.el ends here
