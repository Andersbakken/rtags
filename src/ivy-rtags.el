;;; ivy-rtags.el --- RTags completion back-end for ivy -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2017  Jan Erik Hanssen and Anders Bakken

;; Author: Jan Erik Hanssen <jhanssen@gmail.com>
;;         Anders Bakken <agbakken@gmail.com>
;; URL: https://github.com/Andersbakken/rtags
;; Version: 0.1
;; Package-Requires: ((ivy "0.7.0") (rtags "2.10"))

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
;; Ivy integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'rtags)
(require 'ivy)

(eval-when-compile
  (when (< emacs-major-version 25)
    (defmacro save-mark-and-excursion (&rest body)
      `(save-excursion ,@body))))

(defvar ivy-rtags-tracking-timer nil)

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
    (reverse ret)))

(defun ivy-rtags-select (candidate)
  "Select CANDIDATE."
  (with-current-buffer (get-buffer rtags-buffer-name)
    (goto-char (cdr candidate))
    (rtags-select nil nil)))

;; (defun ivy-rtags-select-other-window (candidate)
;;   (with-current-buffer (get-buffer rtags-buffer-name)
;;     (goto-char (cdr candidate))
;;     (rtags-select t nil)))

(defun ivy-rtags-update ()
  "If `rtags-tracking' is true, follow the selection.
The logic for this function is almost entirely taken from `ivy-call'."
  (when ivy-rtags-tracking-timer
    (cancel-timer ivy-rtags-tracking-timer))
  (when rtags-tracking
    (with-current-buffer (get-buffer rtags-buffer-name)
      (let* ((collection (ivy-state-collection ivy-last))
             (x (cond
                 ;; Alist type.
                 ((and (consp collection)
                       (consp (car collection))
                       (let (idx)
                         (if (setq idx (get-text-property
                                        0 'idx (ivy-state-current ivy-last)))
                             (nth idx collection)
                           (assoc (ivy-state-current ivy-last)
                                  collection)))))
                 (t
                  (ivy-state-current ivy-last)))))
        ;; If x is not a listp, there were no results.
        (when (listp x)
          (setq
           ivy-rtags-tracking-timer
           (run-with-idle-timer
            rtags-tracking-timer-interval
            nil (lambda ()
                  (setq ivy-rtags-tracking-timer nil)
                  (save-mark-and-excursion
                    (with-current-buffer (get-buffer rtags-buffer-name)
                      (select-window (ivy--get-window ivy-last))
                      (prog1 (with-current-buffer (ivy-state-buffer ivy-last)
                               (unwind-protect
                                   (progn
                                     (goto-char (cdr x))
                                     (rtags-select nil nil)))
                               (ivy-recursive-restore)))
                      (unless (or (eq ivy-exit 'done)
                                  (equal (selected-window)
                                         (active-minibuffer-window))
                                  (null (active-minibuffer-window)))
                        (select-window (active-minibuffer-window)))))))))))))

(defun ivy-rtags-read ()
  "RTags completing read function for `ivy'."
  (ivy-read "RTags Ivy: " (ivy-rtags-collection)
            :require-match t
            :action #'ivy-rtags-select
            :update-fn #'ivy-rtags-update))

(provide 'ivy-rtags)

;;; ivy-rtags.el ends here
