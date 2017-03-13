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
;; Ivy integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'rtags)
(require 'ivy)

(defun rtags-ivy-collection ()
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

(defun rtags-ivy-select (candidate)
  (with-current-buffer (get-buffer rtags-buffer-name)
    (goto-char (cdr candidate))
    (rtags-select nil nil)))

;; (defun rtags-ivy-select-other-window (candidate)
;;   (with-current-buffer (get-buffer rtags-buffer-name)
;;     (goto-char (cdr candidate))
;;     (rtags-select t nil)))

(defun rtags-ivy-read ()
  (ivy-read "RTags Ivy: " (rtags-ivy-collection)
            :require-match t
            :action #'rtags-ivy-select))

(provide 'rtags-ivy)
