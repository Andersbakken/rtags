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

(provide 'rtags-code-complete)
(eval-when-compile (require' cl))
(require 'auto-complete)
(require 'rtags)

(defcustom rtags-completions-enabled nil
  "Whether completions with auto-complete are enabled"
  :group 'rtags
  :type 'boolean)

(defvar rtags-last-completions nil) ;; (list "file:12:13" ("foo" "void foo(int)" "bar" "int bar"))

(defun rtags-completion-prefix ()
  (if (or (= (char-before) ?\.)
          (and (= (char-before) ?>) (= (char-before (1- (point))) ?-))
          (and (= (char-before) ?:) (= (char-before (1- (point))) ?:)))
      (point)))

(defun rtags-find-symbol-start () ;; returns column
  (save-excursion
    (if (and (> (point) (point-min)) (looking-at "[ \t]"))
        (backward-char))
    (skip-chars-backward "[A-Za-z0-9_]")
    (if (rtags-completion-prefix)
        (- (point) (point-at-bol))))
  )

(defun rtags-code-complete-at (&optional prepare)
  (interactive)
  (let* ((buffer (current-buffer))
         (modified (buffer-modified-p))
         (text (and modified (buffer-substring-no-properties (point-min) (point-max))))
         (path (buffer-file-name))
         (line (line-number-at-pos))
         (column (1+ (rtags-find-symbol-start))))
    (with-temp-buffer
      (if text
          (insert text))
      (rtags-call-rc :path path :unsaved buffer "-Y" (if prepare "-b" "-l") (format "%s:%d:%d" path line column)))
    )
  )

(defun rtags-update-completions()
  ;; (if rtags-completions-enabled
  ;;     (
  )

(defun rtags-completion-candidates ()
  (message "Candidates called at %s:%d:%d against %s"
           (or (buffer-file-name) (buffer-name))
           (line-number-at-pos)
           (1+ (rtags-find-symbol-start))
           (car rtags-last-completions))
  (let ((loc (format "%s:%d:%d" (or (buffer-file-name) (buffer-name))
                     (line-number-at-pos) (1+ (rtags-find-symbol-start)))))
    (if (string= loc (car rtags-last-completions))
        (let ((completions (cadr rtags-last-completions))
              (completion t)
              (last nil)
              (ret nil))
          (while completions
            (if completion
                (setq last (car completions))
              (setq ret (append ret (list (concat last " - " (car completions))))))
            (setq completion (not completion))
            (setq completions (cdr completions)))
          ret)))
  )

(ac-define-source rtags-completion
  '((init . rtags-diagnostics)
    (prefix . rtags-completion-prefix)
    (candidate . rtags-completion-candidates)))
;;     (prefix . ac-clang-template-prefix)
;;     (requires . 0)
;;     (action . ac-clang-template-action)
;;     (document . ac-clang-document)
;;     (cache)
;;     (symbol . "t")))
