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

(defgroup rtags nil
  "Minor mode for rtags."
  :group 'tools
  :prefix "rtags-")

(require 'bookmark)
(require 'cc-mode)
(require 'cl)
(require 'compile)
(require 'dabbrev)
(require 'ido)
(require 'thingatpt)
(unless (fboundp 'libxml-parse-xml-region)
  (require 'xml))
(unless (require 'auto-complete nil t)
  (defvar ac-sources nil))

(defvar rtags-last-buffer nil)
(defvar rtags-path-filter nil)
(defvar rtags-path-filter-regex nil)
(defvar rtags-range-filter nil)
(defvar rtags-mode-hook nil)
(defvar rtags-taglist-hook nil)
(defface rtags-path nil "Path" :group 'rtags)
(defface rtags-context nil "Context" :group 'rtags)
(defvar rtags-path-face 'rtags-path "Path part")
(defvar rtags-context-face 'rtags-context "Context part")
(defconst rtags-buffer-name "*RTags*")
(defvar rtags-last-request-not-indexed nil)
(defvar rtags-buffer-bookmarks 0)

(defface rtags-warnline
  '((((class color) (background dark)) (:background "blue"))
    (((class color) (background light)) (:background "blue"))
    (t (:bold t)))
  "Face used for marking error lines."
  :group 'rtags)

(defface rtags-errline
  '((((class color) (background dark)) (:background "red"))
    (((class color) (background light)) (:background "red"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'rtags)

(defface rtags-fixitline
  '((((class color) (background dark)) (:background "goldenrod4"))
    (((class color) (background light)) (:background "goldenrod4"))
    (t (:bold t)))
  "Face used for marking fixit lines."
  :group 'rtags)

(defface rtags-local-reference
  '((t :underline t)
    (t (:bold t)))
  "Face used for marking current local references under cursor."
  :group 'rtags)

(defvar rtags-font-lock-keywords
  `((,"^\\(.*:[0-9]+:[0-9]+:\\)\\(.*\\)$"
     (1 font-lock-string-face)
     (2 font-lock-function-name-face))))

(defun rtags-get-buffer (&optional name)
  (unless name (setq name rtags-buffer-name))
  (if (get-buffer name)
      (kill-buffer name))
  (generate-new-buffer name))

(defun rtags-bury-or-delete ()
  (interactive)
  (if (> (length (window-list)) 1)
      (delete-window)
    (bury-buffer)))

(defvar rtags-mode-map nil)
;; assign command to keys
(setq rtags-mode-map (make-sparse-keymap))
(define-key rtags-mode-map (kbd "RET") 'rtags-select-other-window)
(define-key rtags-mode-map (kbd "M-RET") 'rtags-select)
(define-key rtags-mode-map (kbd "ENTER") 'rtags-select-other-window)
(define-key rtags-mode-map (kbd "M-o") 'rtags-show-in-other-window)
(define-key rtags-mode-map (kbd "s") 'rtags-show-in-other-window)
(define-key rtags-mode-map (kbd "SPC") 'rtags-select-and-remove-rtags-buffer)
(define-key rtags-mode-map (kbd "q") 'rtags-bury-or-delete)
(define-key rtags-mode-map (kbd "j") 'next-line)
(define-key rtags-mode-map (kbd "k") 'previous-line)

(define-derived-mode rtags-mode fundamental-mode
  (set (make-local-variable 'font-lock-defaults) '(rtags-font-lock-keywords))
  (setq mode-name "rtags")
  (use-local-map rtags-mode-map)
  (run-hooks 'rtags-mode-hook)
  (goto-char (point-min))
  )

(defun rtags-start-mode (bookmarks readonly)
  (rtags-reset-bookmarks)
  (if bookmarks
      (let ((buf (current-buffer)))
        (goto-char (point-min))
        (while (not (eobp))
          (if (looking-at "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)")
              (let ((file (match-string-no-properties 1))
                    (line (string-to-number (match-string-no-properties 2)))
                    (column (string-to-number (match-string-no-properties 3))))
                (let (deactivate-mark)
                  (with-current-buffer (find-file-noselect file)
                    (goto-char (point-min))
                    (rtags-goto-line-col line column)
                    (setq rtags-buffer-bookmarks (+ rtags-buffer-bookmarks 1))
                    (bookmark-set (format "R_%d" rtags-buffer-bookmarks))
                    (set-buffer buf)))))
          (forward-line))
        ))
  (setq buffer-read-only readonly)
  (rtags-mode))

(defun rtags-reset-bookmarks ()
  (while (> rtags-buffer-bookmarks 0)
    (bookmark-delete (format "R_%d" rtags-buffer-bookmarks))
    (setq rtags-buffer-bookmarks (- rtags-buffer-bookmarks 1)))
  )

(defun rtags-next-match () (interactive) (rtags-next-prev-match t))
(defun rtags-previous-match () (interactive) (rtags-next-prev-match nil))

(defun rtags-next-prev-suitable-match (next)
  (save-excursion
    (if next
        (goto-char (point-at-bol 2))
      (goto-char (point-at-bol 0)))
    (beginning-of-line)
    (when (looking-at "$")
      (when next
        (goto-char (point-min))
        (beginning-of-line)))
    (point))
  )

(defun rtags-next-prev-match (next)
  (if (get-buffer rtags-buffer-name)
      (let (target
            (win (get-buffer-window rtags-buffer-name)))
        (if win
            (select-window win))
        (set-buffer rtags-buffer-name)
        (when (> (count-lines (point-max) (point-min)) 1)
          (cond ((and (= (point-at-bol) (point-min)) (not next))
                 (goto-char (point-max))
                 (beginning-of-line)
                 (while (looking-at "$")
                   (goto-char (1- (point))))
                 (message "%s Wrapped" rtags-buffer-name))
                ((and (= (point-at-eol) (point-max)) next)
                 (goto-char (point-min))
                 (setq target (point-min))
                 (message "%s Wrapped" rtags-buffer-name))
                (t
                 (goto-char (rtags-next-prev-suitable-match next))))
          (beginning-of-line)
          (if win (rtags-select-other-window)
            (rtags-select)))))
  )

(defun rtags-next-diag () (interactive) (rtags-next-prev-diag t))
(defun rtags-previous-diag () (interactive) (rtags-next-prev-diag nil))

(defun rtags-next-prev-diag (next)
  (if (get-buffer "*RTags Diagnostics*")
      (let (target
            (win (get-buffer-window "*RTags Diagnostics*")))
        (if win (select-window win))
        (set-buffer "*RTags Diagnostics*")
        (when (not (= (point-max) (point-min)))
          (cond ((and (= (point-at-bol) (point-min)) (not next))
                 (setq target (- (point-max) 1))
                 (message "*RTags Diagnostics* Wrapped"))
                ((and (>= (+ (point-at-eol) 1) (point-max)) next)
                 (setq target (point-min))
                 (message "*RTags Diagnostics* Wrapped"))
                (next
                 (setq target (point-at-bol 2)))
                (t
                 (setq target (point-at-bol 0))))
          (goto-char target)
          (beginning-of-line)
          (if win (rtags-select-other-window) (rtags-select))))))

(defun rtags-executable-find (exe)
  (let ((result (if rtags-path (concat rtags-path "/bin/" exe) (executable-find exe))))
    (if (and result (file-exists-p result))
        result)))

(defun rtags-remove-keyword-params (seq)
  (if seq
      (let ((head (car seq))
            (tail (cdr seq)))
        (if (keywordp head) (rtags-remove-keyword-params (cdr tail))
          (cons head (rtags-remove-keyword-params tail))))))

(defvar rtags-c++-keywords (make-hash-table :test 'equal))
(clrhash rtags-c++-keywords)
(let ((keywords '("alignas" "alignof" "and" "and_eq" "asm" "auto" "bitand"
                  "bitor" "bool" "break" "case" "catch" "char" "char16_t" "char32_t"
                  "class" "compl" "const" "constexpr" "const_cast" "continue"
                  "decltype" "default" "delete" "do" "double" "dynamic_cast" "else"
                  "enum" "explicit" "export" "extern" "false" "float" "for" "friend"
                  "goto" "if" "inline" "int" "long" "mutable" "namespace" "new" "noexcept"
                  "not" "not_eq" "nullptr" "operator" "or" "or_eq" "private" "protected"
                  "public" "register" "reinterpret_cast" "return" "short" "signed" "sizeof"
                  "static" "static_assert" "static_cast" "struct" "switch" "template" "this"
                  "thread_local" "throw" "true" "try" "typedef" "typeid" "typename" "union"
                  "unsigned" "using" "virtual" "void" "volatile" "wchar_t" "while" "xor" "xor_eq")))
  (while keywords
    (puthash (car keywords) t rtags-c++-keywords)
    (setq keywords (cdr keywords))))

(defun rtags-is-c++-keyword (word)
  (gethash word rtags-c++-keywords))

(defvar rtags-last-context nil)
(defun* rtags-call-rc (&rest arguments
                             &key (path (buffer-file-name))
                             unsaved
                             async ;; nil or a cons (process-filter . sentinel)
                             path-filter
                             path-filter-regex
                             range-filter
                             (output (list t nil)) ; not supported for async
                             context
                             (range-min (1- (point-min)))
                             (range-max (1- (point-max)))
                             noerror
                             &allow-other-keys)
  (setq rtags-last-context context)
  (save-excursion
    (let ((rc (rtags-executable-find "rc")) proc)
      (if (not rc)
          (progn
            (unless noerror (error "Can't find rc"))
            nil)
        (progn
          (and async (not (consp async)) (error "Invalid argument. async must be a cons or nil"))
          (setq arguments (rtags-remove-keyword-params arguments))
          (setq arguments (remove-if '(lambda (arg) (not arg)) arguments))
          (when path-filter
            (push (concat "--path-filter=" path-filter) arguments)
            (if rtags-path-filter-regex
                (push "-Z" arguments)))
          (if unsaved
              (push (format "--unsaved-file=%s:%d"
                            (buffer-file-name unsaved)
                            (with-current-buffer unsaved (- (point-max) (point-min))))
                    arguments))
          (if range-filter
              (push (format "--range-filter=%d-%d" range-min range-max) arguments))
          (if rtags-timeout
              (push (format "--timeout=%d" rtags-timeout) arguments))
          (if (and rtags-show-containing-function (not (member "-N" arguments)))
              (push "-o" arguments))

          (cond ((stringp path) (push (concat "--with-project=" path) arguments))
                (path nil)
                (default-directory (push (concat "--with-project=" default-directory) arguments))
                (t nil))
          (if (and context (not (rtags-is-c++-keyword context)))
              (push (concat "--context=" context) arguments))

          (rtags-log (concat rc " " (combine-and-quote-strings arguments)))
          (let ((proc (cond ((and unsaved async)
                             (let ((proc (apply #'start-process "rc" (current-buffer) rc arguments)))
                               (with-current-buffer unsaved
                                 (process-send-region proc (point-min) (point-max)))
                               proc))
                            (async (apply #'start-process "rc" (current-buffer) rc arguments))
                            ((and unsaved (buffer-modified-p unsaved))
                             (apply #'call-process-region (point-min) (point-max) rc output nil arguments) nil)
                            (unsaved (apply #'call-process rc (buffer-file-name unsaved) output nil arguments) nil)
                            (t (apply #'call-process rc (and unsaved (buffer-file-name unsaved)) output nil arguments) nil))))
            (if proc
                (progn
                  (set-process-query-on-exit-flag proc nil)
                  (set-process-filter proc (car async))
                  (set-process-sentinel proc (cdr async))
                  )
              (progn
                (goto-char (point-min))
                (cond ((looking-at "Can't seem to connect to server")
                       (erase-buffer)
                       (unless noerror
                         (error "Can't seem to connect to server. Is rdm running?")))
                      ((looking-at "Project loading")
                       (erase-buffer)
                       (message "Project loading..."))
                      (t nil)))))
          (or async (> (point-max) (point-min))))))
    )
  )

(defun rtags-index-js-file ()
  (interactive)
  (if (buffer-file-name)
      (let ((bufname (buffer-file-name)))
        (with-temp-buffer
          (rtags-call-rc (buffer-file-name) "--compile" bufname :noerror t))))
  t)

(defvar rtags-preprocess-keymap (make-sparse-keymap))
(define-key rtags-preprocess-keymap (kbd "q") 'rtags-bury-or-delete)
(set-keymap-parent rtags-preprocess-keymap c++-mode-map)
(define-derived-mode rtags-preprocess-mode c++-mode
  (setq mode-name "rtags-preprocess")
  (use-local-map rtags-diagnostics-mode-map)
  (if (buffer-file-name)
      (error "Set buffer with file %s read only " (buffer-file-name)))
  (setq buffer-read-only t)
  )

(defun rtags-builds (&optional file)
  (with-temp-buffer
    (rtags-call-rc :path file "--builds" file)
    (buffer-string))
  )

(defun rtags-preprocess-file (&optional buffer)
  (interactive)
  (unless buffer (setq buffer (current-buffer)))
  (let (narrow-start narrow-end)
    (if (and mark-active
             (not (equal (region-beginning) (region-end))))
        (setq narrow-start (+ 1 (count-lines (point-min) (region-beginning)))
              narrow-end (+ 1 (count-lines (point-min) (region-end)))))
    (let ((preprocess-buffer (rtags-get-buffer (format "*RTags preprocessed %s*" (buffer-file-name buffer)))))
      (rtags-location-stack-push)
      (with-current-buffer preprocess-buffer
        (rtags-call-rc :path (buffer-file-name buffer) "--preprocess" (buffer-file-name buffer))
        (if (and narrow-start narrow-end)
            (let ((match-regexp (concat "^# \\([0-9]*\\) \"" (file-truename (buffer-file-name buffer)) "\""))
                  last-match last-line start end)
              (while (re-search-forward match-regexp nil t)
                (let ((current-line (string-to-number (match-string-no-properties 1))))
                  (if (and (not start) (> current-line narrow-start))
                      (setq start (+ (count-lines (point-min) last-match) (- narrow-start last-line))))
                  (if (and (not end) (> current-line narrow-end))
                      (setq end (+ (count-lines (point-min) last-match) (- narrow-end last-line))))
                  (setq last-line current-line)
                  (setq last-match (point))))
              (if last-match
                  (progn
                    (if (not start)
                        (setq start (+ (count-lines (point-min) last-match) (- narrow-start last-line))))
                    (if (not end)
                        (setq end (+ (count-lines (point-min) last-match) (- narrow-end last-line))))))
              (if (and start end)
                  (progn
                    (goto-char (point-min))
                    (narrow-to-region (point-at-bol (+ start 1)) (point-at-bol (+ end 1)))))))
        (rtags-preprocess-mode))
      (display-buffer preprocess-buffer))
    ))

(defun rtags-reparse-file (&optional buffer)
  (interactive)
  (let ((file (buffer-file-name buffer)))
    (when file
      (with-temp-buffer
        (rtags-call-rc :path file "-V" file))
      (message (format "Dirtied %s" file)))
    )
  )

;; /home/abakken/dev (loaded) <=

(defun rtags-set-current-project ()
  (interactive)
  (let ((projects nil)
        (project nil)
        (current ""))
    (with-temp-buffer
      (rtags-call-rc :path t "-w")
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
          (if (string-match "^\\([^ ]+\\)[^<]*<=$" line)
              (let ((name (match-string-no-properties 1 line)))
                (setq projects (add-to-list 'projects name t))
                (setq current name))
            (if (string-match "^\\([^ ]+\\)[^<]*$" line)
                (setq projects (add-to-list 'projects (match-string-no-properties 1 line))))))
        (forward-line))
      )
    (setq project (ido-completing-read
                   (format "RTags select project (current is %s): " current)
                   projects))
    (if project
        (with-temp-buffer (rtags-call-rc :output nil :path t "-w" project)))
    )
  )

(defun rtags-find-ancestor-file (pattern)
  "Find a file named \a file in as shallow a path as possible,
  e.g. if there's a Makefile in /foobar/rtags/rc/Makefile and one
  in /foobar/rtags/Makefile it will return the latter. Wildcards
  are allowed. If multiple files match return first match. "
  (let ((best nil)
        (dir default-directory))
    (while (cond ((string= dir "") nil)
                 ((string= dir "/") nil)
                 (t t))
      (let ((match (file-expand-wildcards (concat dir pattern))))
        (if match
            (setq best (nth 0 match))))
      (setq dir (substring dir 0 (string-match "[^/]*/?$" dir))))
    best))

(defun rtags-find-ancestor-file-directory (pattern)
  (let ((match (rtags-find-ancestor-file pattern)))
    (if match
        (file-name-directory match))))

(defun rtags-default-current-project ()
  (cond
   ((gtags-get-rootpath))
   ((git-root-dir))
   ((rtags-find-ancestor-file-directory "configure"))
   ((rtags-find-ancestor-file-directory "CMakeLists.txt"))
   ((rtags-find-ancestor-file-directory "*.pro"))
   ((rtags-find-ancestor-file-directory "scons.1")) ;; Is this the right way to determine this?
   ((rtags-find-ancestor-file-directory "autogen.*"))
   ((rtags-find-ancestor-file-directory "Makefile*"))
   ((rtags-find-ancestor-file-directory "INSTALL*"))
   ((rtags-find-ancestor-file-directory "README*"))
   (t nil)))

(defun rtags-current-symbol (&optional no-symbol-name)
  (or (and mark-active (buffer-substring-no-properties (point) (mark)))
      (and (not no-symbol-name) (rtags-current-symbol-name))
      (thing-at-point 'symbol)))

(defun rtags-cursorinfo (&optional location verbose)
  (let ((loc (or location (rtags-current-location)))
        (context (unless location (rtags-current-symbol t)))
        (path (buffer-file-name)))
    (with-temp-buffer
      (rtags-call-rc :path path
                     :context context
                     "-U" loc
                     (if verbose "--cursorinfo-include-targets")
                     (if verbose "--cursorinfo-include-references"))
      (buffer-string))))

(defun rtags-print-cursorinfo (&optional verbose)
  (interactive "P")
  (message "%s" (rtags-cursorinfo nil verbose)))

(defun rtags-print-dependencies (&optional buffer)
  (interactive)
  (let ((dep-buffer (rtags-get-buffer))
        (fn (buffer-file-name (or buffer (current-buffer)))))
    (rtags-location-stack-push)
    (switch-to-buffer dep-buffer)
    (rtags-call-rc :path fn "--dependencies" fn)
    (rtags-start-mode nil t)))

(defun rtags-print-enum-value-at-point (&optional location)
  (interactive)
  (let ((info (rtags-cursorinfo location)))
    (cond ((string-match "^Enum Value: \\([0-9]+\\) *$" info)
           (let ((enumval (match-string-no-properties 1 info)))
             (message "%s - %s - 0x%X" (rtags-current-symbol-name info) enumval (string-to-number enumval))))
          ((string-match "^Type: Enum *$" info)
           (let ((target (rtags-target)))
             (when target
               (setq info (rtags-cursorinfo target))
               (if (string-match "^Enum Value: \\([0-9]+\\) *$" info)
                   (let ((enumval (match-string-no-properties 1 info)))
                     (message "%s - %s - 0x%X" (rtags-current-symbol-name info) enumval (string-to-number enumval)))))))
          (t (message "RTags: No enum here") nil))))

(defun rtags-buffer-is-multibyte ()
  (string-match "\\butf\\b" (symbol-name buffer-file-coding-system)))

(defun rtags-buffer-is-dos()
  (string-match "\\bdos\\b" (symbol-name buffer-file-coding-system)))

(defun rtags-carriage-returns ()
  (if (rtags-buffer-is-dos)
      (1- (line-number-at-pos))
    0)
  )

(defun rtags-offset (&optional p)
  (let (carriagereturns)
    (save-excursion
      (if p
          (goto-char p)
        (setq carriagereturns (rtags-carriage-returns))
        (if (rtags-buffer-is-multibyte)
            (let ((prev (buffer-local-value enable-multibyte-characters (current-buffer)))
                  (loc (local-variable-p enable-multibyte-characters))
                  (pos))
              (set-buffer-multibyte nil)
              (setq pos (1- (point)))
              (set-buffer-multibyte prev)
              (unless loc
                (kill-local-variable enable-multibyte-characters))
              (+ pos carriagereturns))
          (+ (1- (point)) carriagereturns))))))

(defun rtags-goto-offset (pos)
  (interactive "NOffset: ")
  (if (rtags-buffer-is-multibyte)
      (let ((prev (buffer-local-value enable-multibyte-characters (current-buffer)))
            (loc (local-variable-p enable-multibyte-characters)))
        (set-buffer-multibyte nil)
        (goto-char (1+ pos))
        (set-buffer-multibyte prev)
        (unless loc
          (kill-local-variable enable-multibyte-characters)))
    (goto-char (1+ pos))))

(defun rtags-current-location (&optional offset)
  (if offset
      (format "%s,%d" (or (buffer-file-name) (buffer-name)) (rtags-offset))
    (format "%s:%d:%d" (or (buffer-file-name) (buffer-name))
            (line-number-at-pos) (1+ (- (point) (point-at-bol)))))
  )

(defun rtags-log (log)
  (if rtags-rc-log-enabled
      (with-current-buffer (get-buffer-create "*RTags Log*")
        (goto-char (point-max))
        (setq buffer-read-only nil)
        (insert "**********************************\n" log "\n")
        (setq buffer-read-only t)
        )
    )
  )

(defvar rtags-symbol-history nil)

(defun rtags-save-location ()
  (setq rtags-last-buffer (current-buffer))
  (rtags-location-stack-push))

(defun rtags-find-file-or-buffer (file-or-buffer &optional other-window)
  (if (file-exists-p file-or-buffer)
      (if other-window
          (find-file-other-window file-or-buffer)
        (find-file file-or-buffer))
    (let ((buf (get-buffer file-or-buffer)))
      (cond ((not buf) (message "No buffer named %s" file-or-buffer))
            (other-window (switch-to-buffer-other-window file-or-buffer))
            (t (switch-to-buffer file-or-buffer))))
    )
  )

(defun rtags-find-context-on-line ()
  (if rtags-last-context
      (cond ((search-forward rtags-last-context (point-at-eol) t)
             (backward-char (length rtags-last-context)))
            ((search-backward rtags-last-context (point-at-bol) t))
            (t)))
  )

(defun rtags-goto-line-col (line column)
  (goto-char (point-min))
  (forward-line (1- line))
  (beginning-of-line)
  (forward-char (- column 1))
  )

(defun rtags-goto-location (location &optional nobookmark other-window)
  "Go to a location passed in. It can be either: file,12 or file:13:14 or plain file"
  ;; (message (format "rtags-goto-location \"%s\"" location))
  (when (> (length location) 0)
    (cond ((string-match "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)" location)
           (let ((line (string-to-number (match-string-no-properties 2 location)))
                 (column (string-to-number (match-string-no-properties 3 location))))
             (rtags-find-file-or-buffer (match-string-no-properties 1 location) other-window)
             (run-hooks rtags-after-find-file-hook)
             (rtags-goto-line-col line column)
             (rtags-find-context-on-line)
             t))
          ((string-match "\\(.*\\):\\([0-9]+\\)" location)
           (let ((line (string-to-number (match-string-no-properties 2 location))))
             (rtags-find-file-or-buffer (match-string-no-properties 1 location) other-window)
             (run-hooks rtags-after-find-file-hook)
             (goto-char (point-min))
             (forward-line (1- line))
             (rtags-find-context-on-line)
             t))
          ((string-match "\\(.*\\),\\([0-9]+\\)" location)
           (let ((offset (string-to-number (match-string-no-properties 2 location))))
             (rtags-find-file-or-buffer (match-string-no-properties 1 location) other-window)
             (run-hooks rtags-after-find-file-hook)
             (rtags-goto-offset offset)
             (rtags-find-context-on-line)
             t))
          (t
           (if (string-match "^ +\\(.*\\)$" location)
               (setq location (match-string-no-properties 1 location)))
           (rtags-find-file-or-buffer location other-window)))
    (unless nobookmark (rtags-location-stack-push))
    )
  )

(defvar rtags-async-filter-and-sentinel (cons 'rtags-async-rc-filter 'rtags-async-rc-sentinel))

(defun rtags-find-symbols-by-name-internal (prompt references filter)
  (rtags-save-location)
  (let ((tagname (if mark-active
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (rtags-current-symbol)))
        (switch (if references "-R" "-F"))
        (path (buffer-file-name))
        input)
    (if (> (length tagname) 0)
        (setq prompt (concat prompt ": (default " tagname ") "))
      (setq prompt (concat prompt ": ")))
    (setq input (completing-read prompt (function rtags-symbolname-complete) nil nil nil 'rtags-symbol-history))
    (setq rtags-symbol-history (remove-duplicates rtags-symbol-history :from-end t :test 'equal))
    (if (not (equal "" input))
        (setq tagname input))
    (with-current-buffer (rtags-get-buffer)
      (if references
          (setq references rtags-async-filter-and-sentinel))
      (rtags-call-rc :path path switch tagname :path-filter filter :context tagname)
      (rtags-reset-bookmarks)
      (rtags-handle-results-buffer))
    )
  )

(defun rtags-remove-results-buffer ()
  (interactive)
  (kill-buffer (current-buffer))
  (switch-to-buffer rtags-last-buffer))

(defun rtags-symbolname-completion-get (string)
  (with-temp-buffer
    (rtags-call-rc "-Y" "-S" string)
    (eval (read (buffer-string)))))

(defun rtags-symbolname-completion-exactmatch (string)
  (with-temp-buffer
    (rtags-call-rc "-N" "-F" string)
    (> (point-max) (point-min))))

(defun rtags-symbolname-complete (string predicate code)
  (cond ((eq code nil)
         (try-completion string (rtags-symbolname-completion-get string) predicate))
        ((eq code t) (rtags-symbolname-completion-get string))
        ((eq code 'lambda) (rtags-symbolname-completion-exactmatch string))))

(defvar rtags-location-stack-index 0)
(defvar rtags-location-stack nil)

(defun rtags-location-stack-push ()
  (let ((bm (rtags-current-location)))
    (while (> rtags-location-stack-index 0)
      (progn
        (setq rtags-location-stack-index (- rtags-location-stack-index 1))
        (pop rtags-location-stack)
        )
      )
    (unless (string= bm (nth 0 rtags-location-stack))
      (push bm rtags-location-stack)
      (if (> (length rtags-location-stack) rtags-max-bookmark-count)
          (nbutlast rtags-location-stack (- (length rtags-location-stack) rtags-max-bookmark-count))
        )
      )
    )
  )

(defun rtags-location-stack-jump (by)
  (interactive)
  (let ((instack (nth rtags-location-stack-index rtags-location-stack))
        (cur (rtags-current-location)))
    (if (not (string= instack cur))
        (rtags-goto-location instack t)
      (let ((target (+ rtags-location-stack-index by)))
        (if (and (>= target 0) (< target (length rtags-location-stack)))
            (progn
              (setq rtags-location-stack-index target)
              (rtags-goto-location (nth rtags-location-stack-index rtags-location-stack) t)
              )
          )
        )
      )
    )
  )

;; **************************** API *********************************

(defcustom rtags-enabled t
  "Whether rtags is enabled. We try to do nothing when it's not"
  :group 'rtags
  :type 'boolean)

(defcustom rtags-tracking nil
  "When on automatically jump to symbol under cursor in *RTags* buffer"
  :group 'rtags
  :type 'boolean)

(defcustom rtags-error-timer-interval .5
  "Interval for minibuffer error timer"
  :group 'rtags
  :type 'number)

(defcustom rtags-display-current-error-as-message t
  "Display error under cursor using (message)"
  :type 'boolean
  :group 'rtags)

(defcustom rtags-display-current-error-as-tooltip nil
  "Display error under cursor using popup-tip (requires 'popup)"
  :type 'boolean
  :group 'rtags)

(defcustom rtags-error-timer-interval .5
  "Interval for minibuffer error timer"
  :group 'rtags
  :type 'number)

(defcustom rtags-local-references-enabled nil
  "Whether rtags local-references are enabled"
  :group 'rtags
  :type 'boolean)

(defcustom rtags-local-references-timer-interval .5
  "Interval for local-references timer"
  :group 'rtags
  :type 'number)

(defcustom rtags-tracking-timer-interval .5
  "Interval for tracking timer"
  :group 'rtags
  :type 'number)

(defcustom rtags-expand-function '(lambda () (dabbrev-expand nil))
  "What function to call for expansions"
  :group 'rtags
  :type 'function)

(defcustom rtags-after-find-file-hook nil
  "Run after rtags has jumped to a location possibly in a new file"
  :group 'rtags
  :type 'hook)

(defcustom rtags-mode-hook nil
  "Run when rtags-mode is started"
  :group 'rtags
  :type 'hook)

(defcustom rtags-edit-hook nil
  "Run before rtags tries to modify a buffer (from rtags-rename)
return t if rtags is allowed to modify this file"
  :group 'rtags
  :type 'hook)

(defcustom rtags-jump-to-first-match t
  "If t, jump to first match"
  :group 'rtags
  :type 'boolean)

(defcustom rtags-timeout nil
  "Max amount of ms to wait before timing out requests"
  :group 'rtags
  :type 'integer)

(defcustom rtags-path nil
  "Path to rtags executables"
  :group 'rtags
  :type 'string)

(defcustom rtags-max-bookmark-count 100
  "How many bookmarks to keep in stack"
  :group 'rtags
  :type 'integer)

(defcustom rtags-rc-log-enabled nil
  "If t, log rc commands and responses"
  :group 'rtags
  :type 'boolean)

(defcustom rtags-show-containing-function nil
  "If t, pass -o to rc to include containing function"
  :group 'rtags
  :type 'boolean)

(defcustom rtags-index-js-files nil
  "If t, automatically index all js files that are opened"
  :group 'rtags
  :type 'boolean)

(defun rtags-enable-standard-keybindings (&optional map prefix)
  (interactive)
  (unless map
    (setq map c-mode-base-map))
  (unless prefix
    (setq prefix "C-x r"))
  (define-key map (format "%s ." prefix) (function rtags-find-symbol-at-point))
  (define-key map (format "%s ," prefix) (function rtags-find-references-at-point))
  (define-key map (format "%s v" prefix) (function rtags-find-virtuals-at-point))
  (define-key map (format "%s V" prefix) (function rtags-print-enum-value-at-point))
  (define-key map (format "%s /" prefix) (function rtags-find-all-references-at-point))
  (define-key map (format "%s Y" prefix) (function rtags-cycle-overlays-on-screen))
  (define-key map (format "%s >" prefix) (function rtags-find-symbol))
  (define-key map (format "%s <" prefix) (function rtags-find-references))
  (define-key map (format "%s [" prefix) (function rtags-location-stack-back))
  (define-key map (format "%s ]" prefix) (function rtags-location-stack-forward))
  (define-key map (format "%s D" prefix) (function rtags-diagnostics))
  (define-key map (format "%s G" prefix) (function rtags-guess-function-at-point))
  (define-key map (format "%s p" prefix) (function rtags-set-current-project))
  (define-key map (format "%s P" prefix) (function rtags-print-dependencies))
  (define-key map (format "%s e" prefix) (function rtags-reparse-file))
  (define-key map (format "%s E" prefix) (function rtags-preprocess-file))
  (define-key map (format "%s R" prefix) (function rtags-rename-symbol))
  (define-key map (format "%s U" prefix) (function rtags-print-cursorinfo))
  (define-key map (format "%s O" prefix) (function rtags-goto-offset))
  (define-key map (format "%s ;" prefix) (function rtags-find-file))
  (define-key map (format "%s F" prefix) (function rtags-fixit))
  (define-key map (format "%s x" prefix) (function rtags-fix-fixit-at-point))
  (define-key map (format "%s B" prefix) (function rtags-show-rtags-buffer))
  (define-key map (format "%s I" prefix) (function rtags-imenu))
  (define-key map (format "%s T" prefix) (function rtags-taglist))
  )

(if rtags-index-js-files
    (add-hook 'find-file-hook 'rtags-index-js-file)
  (remove-hook 'find-file-hook 'rtags-index-js-file))

(defun rtags-print-current-location ()
  (interactive)
  (message (rtags-current-location)))

(defun rtags-quit-rdm () (interactive)
  (call-process (rtags-executable-find "rc") nil nil nil "--quit-rdm"))

(defun rtags-location-stack-forward ()
  (interactive)
  (rtags-location-stack-jump -1)
  )

(defun rtags-location-stack-back ()
  (interactive)
  (rtags-location-stack-jump 1)
  )

(defun rtags-location-stack-reset ()
  (interactive)
  (setq rtags-location-stack nil)
  (setq rtags-location-stack-index 0)
  )

(defun rtags-target (&optional filter)
  (let ((path (buffer-file-name))
        (location (rtags-current-location))
        (context (rtags-current-symbol t)))
    (if path
        (with-temp-buffer
          (rtags-call-rc :path path "-N" "-f" location :context context :path-filter filter :noerror t)
          (setq rtags-last-request-not-indexed nil)
          (cond ((= (point-min) (point-max))
                 (message "RTags: No target") nil)
                ((or (string= (buffer-string) "Not indexed\n")
                     (string= (buffer-string) "Can't seem to connect to server\n"))
                 (setq rtags-last-request-not-indexed t) nil)
                (t (buffer-substring-no-properties (point-min) (- (point-max) 1))))
          )
      )
    )
  )

;; (defalias 'rtags-find-symbol-at-point 'rtags-follow-symbol-at-point)
(defun rtags-find-symbol-at-point (&optional prefix)
  "Find the natural target for the symbol under the cursor and moves to that location.
For references this means to jump to the definition/declaration of the referenced symbol (it jumps to the definition if it is indexed).
For definitions it jumps to the declaration (if there is only one) For declarations it jumps to the definition.
If called with a prefix restrict to current buffer"
  (interactive "P")
  (rtags-save-location)
  (let ((target (rtags-target prefix)))
    (if target
        (rtags-goto-location target))
    )
  )

(defun rtags-find-references-at-point (&optional prefix)
  "Find all references to the symbol under the cursor
If there's exactly one result jump directly to it.
If there's more show a buffer with the different alternatives and jump to the first one if rtags-jump-to-first-match is true.
References to references will be treated as references to the referenced symbol"
  (interactive "P")
  (rtags-save-location)
  (let ((arg (rtags-current-location))
        (fn (buffer-file-name))
        (context (rtags-current-symbol t)))
    (with-current-buffer (rtags-get-buffer)
      (rtags-call-rc :path fn :context context :path-filter prefix "-r" arg)
      (rtags-handle-results-buffer))
    )
  )

(defun rtags-find-virtuals-at-point (&optional prefix)
  "List all reimplentations of function under cursor. This includes both declarations and definitions"
  (interactive "P")
  (rtags-save-location)
  (let ((arg (rtags-current-location))
        (fn (buffer-file-name))
        (context (rtags-current-symbol t)))
    (with-current-buffer (rtags-get-buffer)
      (rtags-call-rc :path fn :context context :path-filter prefix "-r" arg "-k")
      (rtags-handle-results-buffer))
    )
  )

(defun rtags-find-all-references-at-point (&optional prefix)
  (interactive "P")
  (rtags-save-location)
  (let ((arg (rtags-current-location))
        (fn (buffer-file-name))
        (context (rtags-current-symbol t)))
    (with-current-buffer (rtags-get-buffer)
      (rtags-call-rc :path fn :context context :path-filter prefix "-r" arg "-e")
      (rtags-handle-results-buffer))
    )
  )

(defun rtags-guess-function-at-point()
  (interactive)
  (rtags-save-location)
  (let ((token (rtags-current-token))
        (fn (buffer-file-name))
        (context (rtags-current-symbol t)))
    (if token
        (with-current-buffer (rtags-get-buffer)
          (rtags-call-rc :path fn "--declaration-only" "-F" token)
          (rtags-handle-results-buffer t))))
  )

(defun rtags-current-token ()
  (save-excursion
    (when (looking-at "[0-9A-Za-z_~#]")
      (while (and (> (point) (point-min)) (looking-at "[0-9A-Za-z_~#]"))
        (backward-char))
      (if (not (looking-at "[0-9A-Za-z_~#]"))
          (forward-char))
      (let ((start (point)))
        (while (looking-at "[0-9A-Za-z_~#]")
          (forward-char))
        (buffer-substring-no-properties start (point))))))

(defun rtags-rename-symbol ()
  (interactive)
  (save-some-buffers) ;; it all kinda falls apart when buffers are unsaved
  (let (location len file pos destructor replacewith prev (modifications 0) (filesopened 0) replacements buffers)
    (save-excursion
      (if (looking-at "[0-9A-Za-z_~#]")
          (progn
            (while (and (> (point) (point-min)) (looking-at "[0-9A-Za-z_~#]"))
              (backward-char))
            (if (not (looking-at "[0-9A-Za-z_~#]"))
                (forward-char))
            (setq file (buffer-file-name (current-buffer)))
            (setq pos (point))
            (if (looking-at "~")
                (progn
                  (setq pos (+ pos 1))
                  (setq destructor t)))
            (while (looking-at "[0-9A-Za-z_~#]")
              (forward-char))
            (setq prev (buffer-substring-no-properties pos (point)))
            (setq len (- (point) pos))
            (setq replacewith (read-from-minibuffer (format "Replace '%s' with: " prev)))
            (unless (equal replacewith "")
              (if destructor
                  (setq pos (- pos 1)))
              (goto-char pos)
              (setq location (rtags-current-location))
              (setq pos (rtags-offset pos))
              (with-temp-buffer
                (rtags-call-rc :path file "-e" "-O" "-N" "-r" location)
                ;; (message "Got renames %s" (buffer-string))
                (dolist (line (split-string (buffer-string) "\n" t))
                  (if (string-match "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):$" line)
                      (add-to-list 'replacements (cons (match-string-no-properties 1 line)
                                                       (cons (string-to-number (match-string-no-properties 2 line))
                                                             (string-to-number (match-string-no-properties 3 line)))) t))))
              ;; (message "Got %d replacements" (length replacements))

              (dolist (value replacements)
                (let ((buf (find-buffer-visiting (car value))))
                  (unless buf
                    (progn
                      (incf filesopened)
                      (setq buf (find-file-noselect (car value)))))
                  (when buf
                    (set-buffer buf)
                    (add-to-list 'buffers buf)
                    (when (run-hook-with-args-until-failure 'rtags-edit-hook)
                      (incf modifications)
                      (rtags-goto-line-col (cadr value) (cddr value))
                      (if (looking-at "~")
                          (forward-char))

                      ;; (message "About to replace %s with %s at %d in %s"
                      ;;          (buffer-substring-no-properties (point) (+ (point) len)) replacewith (point) (car value))
                      (delete-char len)
                      (insert replacewith)))))))))
    (dolist (value buffers)
      (with-current-buffer value
        (basic-save-buffer)))
    (message (format "Opened %d new files and made %d modifications" filesopened modifications)))
  )

(defun rtags-find-symbol (&optional prefix)
  (interactive "P")
  (rtags-find-symbols-by-name-internal "Find rsymbol" nil prefix))

(defun rtags-find-references (&optional prefix)
  (interactive "P")
  (rtags-find-symbols-by-name-internal "Find rreferences" t prefix))

(defun rtags-find-symbol-current-file ()
  (interactive)
  (rtags-find-symbol t))

(defun rtags-find-references-current-file ()
  (interactive)
  (rtags-find-references t))

(defun rtags-dir-filter ()
  (concat (substring buffer-file-name
                     0
                     (string-match
                      "[^/]*/?$"
                      buffer-file-name))
          "[^/]*/?$"))

(defun rtags-find-symbol-current-dir ()
  (interactive)
  (setq rtags-path-filter-regex t)
  (rtags-find-symbols-by-name-internal "Find rsymbol" nil (rtags-dir-filter))
  (setq rtags-path-filter-regex nil))

(defun rtags-find-references-current-dir ()
  (interactive)
  (setq rtags-path-filter-regex t)
  (rtags-find-symbols-by-name-internal "Find rreferences" t (rtags-dir-filter))
  (setq rtags-path-filter-regex nil))

(defvar rtags-diagnostics-process nil)
(defun rtags-apply-fixit-at-point ()
  (interactive)
  (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
    (if (string-match "^\\(.*\\):[0-9]+:[0-9]+: fixit: \\([0-9]+\\)-\\([0-9]+\\): .*did you mean '\\(.*\\)'\\?$" line)
        (let* ((file (match-string-no-properties 1 line))
               (buf (find-buffer-visiting file))
               (start (string-to-number (match-string-no-properties 2 line)))
               (end (string-to-number (match-string-no-properties 3 line)))
               (text (match-string-no-properties 4 line)))
          (unless buf
            (setq buf (find-file-noselect file)))
          (when (and buf
                     (or (not (buffer-modified-p buf))
                         (y-or-n-p (format "%s is modified. This is probably not a good idea. Are you sure? " file))))
            (let ((win (get-buffer-window buf)))
              (if win
                  (select-window win)
                (switch-to-buffer-other-window buf)))
            (save-excursion
              (rtags-goto-offset start)
              (delete-char (- end start)) ;; may be 0
              (insert text)))))))

(defvar rtags-overlays (make-hash-table :test 'equal))

(defun rtags-overlays-remove (filename)
  (let ((errorlist (gethash filename rtags-overlays nil)))
    (while (and errorlist (listp errorlist))
      (delete-overlay (car errorlist))
      (setq errorlist (cdr errorlist)))
    (puthash filename nil rtags-overlays)))

(defun rtags-clear-diagnostics-overlays()
  (interactive)
  (if (buffer-file-name)
      (rtags-overlays-remove (buffer-file-name)))
  )

(defun rtags-really-find-buffer (fn)
  (setq fn (file-truename fn))
  (car
   (member-if #'(lambda (arg)
                  (and (buffer-file-name arg)
                       (string= fn (file-truename (buffer-file-name arg)))))
              (buffer-list))))

(defun rtags-string-to-number (string)
  (when (stringp string)
    (string-to-number string)))

(defun rtags-parse-xml-string (xml)
  (with-temp-buffer
    (insert xml)
    (if (fboundp 'libxml-parse-xml-region)
        (libxml-parse-xml-region (point-min) (point-max))
      (car (xml-parse-region (point-min) (point-max))))))

(defun rtags-parse-overlay-error-node (node filename)
  (when (listp node)
    (let* ((name (car node))
           (attrs (cadr node))
           (line (rtags-string-to-number (cdr (assq 'line attrs))))
           (column (rtags-string-to-number (cdr (assq 'column attrs))))
           (startoffset (rtags-string-to-number (cdr (assq 'startOffset attrs))))
           (endoffset (rtags-string-to-number (cdr (assq 'endOffset attrs))))
           (severity (cdr (assq 'severity attrs)))
           (message (cdr (assq 'message attrs))))
      (when (eq name 'error)
        (let ((errorlist (gethash filename rtags-overlays nil))
              (filebuffer (rtags-really-find-buffer filename)))
          (when filebuffer
            (when (or (not endoffset) (= endoffset -1))
              (with-current-buffer filebuffer
                (save-excursion
                  (if startoffset
                      (rtags-goto-offset startoffset)
                    (progn
                      (rtags-goto-line-col line column)
                      (setq startoffset (rtags-offset))))
                  (let ((rsym (rtags-current-symbol t)))
                    (when rsym
                      (setq endoffset (+ startoffset (length rsym))))))))

            (if (and startoffset endoffset filebuffer)
                (let ((overlay (make-overlay (1+ startoffset)
                                             (cond ((= startoffset endoffset) (+ startoffset 2))
                                                   (t (1+ endoffset)))
                                             filebuffer)))
                  (overlay-put overlay 'rtags-error-message message)
                  (overlay-put overlay 'rtags-error-severity severity)
                  (overlay-put overlay 'rtags-error-start startoffset)
                  (overlay-put overlay 'rtags-error-end endoffset)
                  (overlay-put overlay 'face (cond ((string= severity "error") 'rtags-errline)
                                                   ((string= severity "warning") 'rtags-warnline)
                                                   ((string= severity "fixit") 'rtags-fixitline)
                                                   (t 'rtags-errline)))
                  (if (string= severity "fixit")
                      (progn
                        (overlay-put overlay 'priority 1)
                        (insert (format "%s:%d:%d: fixit: %d-%d: %s\n" filename line column startoffset endoffset message)))
                    (insert (format "%s:%d:%d: %s: %s\n" filename line column severity message)))

                  (setq errorlist (append errorlist (list overlay)))
                  (puthash filename errorlist rtags-overlays))))))))
  )

(defun rtags-parse-overlay-node (node)
  (when (listp node)
    (let* ((name (car node))
           (attrs (cadr node))
           (body (cddr node))
           (filename (cdr (assq 'name attrs))))
      (when (eq name 'file)
        (rtags-overlays-remove filename)
        (save-excursion
          (goto-char (point-min))
          (flush-lines (concat filename ":")))
        (dolist (it body)
          (rtags-parse-overlay-error-node it filename))))
    )
  )

(defvar rtags-last-index nil)
(defvar rtags-last-total nil)

(defun rtags-modeline-progress ()
  (if (and rtags-last-index
           rtags-last-total
           (> rtags-last-total 0))
      ;; (not (= rtags-last-index rtags-last-total)))
      (format "RTags: %d/%d %d%%%% " rtags-last-index rtags-last-total (/ (* rtags-last-index 100) rtags-last-total))
    ""))

(add-to-list 'global-mode-string '(:eval (rtags-modeline-progress)))
(defun rtags-parse-diagnostics (output)
  (let ((doc (rtags-parse-xml-string output)) body)
    (when doc
      (cond ((eq (car doc) 'checkstyle)
             (setq body (cddr doc))
             (while body
               (rtags-parse-overlay-node (car body))
               (setq body (cdr body))))
            ((eq (car doc) 'completions)
             (message "Got completions [%s]" body)
             (setq body (car (cddr doc)))
             (if (boundp 'rtags-last-completions
                         (setq rtags-last-completions
                               (list (cdar (cadr doc)) ;; location attribute
                                     (eval (read (substring body 8 (- (length body) 2)))))))))
            ((eq (car doc) 'progress)
             (setq body (cadr doc))
             (while body
               (cond ((eq (caar body) 'index)
                      ;; (message "Got index [%s]" (cdar body))
                      (setq rtags-last-index (string-to-number (cdar body))))
                     ((eq (caar body) 'total)
                      (setq rtags-last-total (string-to-number (cdar body))))
                     (t (message "Unexpected element %s" (caar body))))
               (setq body (cdr body)))
             (force-mode-line-update))
            ;;             (message "RTags: %s/%s (%s%%)" index total)))
            (t (message "Unexpected root element %s" (car doc)))
            )
      )
    )
  )

(defun rtags-check-overlay (overlay)
  (if (and (not (active-minibuffer-window)) (not cursor-in-echo-area))
      (rtags-display-overlay overlay (point))
    )
  )

(defun rtags-is-running ()
  (interactive)
  (with-temp-buffer
    (rtags-call-rc "--is-indexing" :noerror t)))

(defun rtags-display-overlay (overlay point)
  (let ((msg (overlay-get overlay 'rtags-error-message)))
    (when (stringp msg)
      (if rtags-display-current-error-as-tooltip
          (popup-tip msg :point point)) ;; :face 'rtags-warnline)) ;;(overlay-get overlay 'face)))
      (if rtags-display-current-error-as-message
          (message (concat "RTags: " msg)))))
  )

(defvar rtags-update-current-error-timer nil)

(defun rtags-display-current-error ()
  (let ((current-overlays (overlays-at (point))))
    (setq rtags-update-current-error-timer nil)
    (while (and current-overlays (not (rtags-check-overlay (car current-overlays))))
      (setq current-overlays (cdr current-overlays))))
  )

(defun rtags-update-current-error ()
  (if rtags-update-current-error-timer
      (cancel-timer rtags-update-current-error-timer))
  (setq rtags-update-current-error-timer
        (and (or rtags-display-current-error-as-message
                 rtags-display-current-error-as-tooltip)
             (get-buffer "*RTags Diagnostics*")
             (run-with-idle-timer
              rtags-error-timer-interval
              nil
              (function rtags-display-current-error))))
  )

(defun rtags-is-rtags-overlay (overlay) (and overlay (overlay-get overlay 'rtags-error-message)))

(defun rtags-overlay-comparator (l r)
  (< (overlay-start l) (overlay-start r)))

(defun rtags-overlays-on-screen ()
  (sort (remove-if-not 'rtags-is-rtags-overlay (overlays-in (window-start) (window-end))) #'rtags-overlay-comparator))

(defvar rtags-highlighted-overlay nil)

(defun rtags-cycle-overlays-on-screen ()
  (interactive)
  (let* ((overlays (rtags-overlays-on-screen))
         (idx (and rtags-highlighted-overlay (position rtags-highlighted-overlay overlays)))
         (overlay (if (and idx (< (1+ idx) (length overlays)))
                      (nth (1+ idx) overlays)
                    (car overlays))))
    (when overlay
      (setq rtags-highlighted-overlay overlay)
      (rtags-display-overlay overlay (overlay-start overlay))))
  )

(defun rtags-fix-fixit-overlay (overlay)
  (let ((msg (overlay-get overlay 'rtags-error-message))
        (severity (overlay-get overlay 'rtags-error-severity))
        (insert)
        (start (overlay-get overlay 'rtags-error-start))
        (end (overlay-get overlay 'rtags-error-end)))
    (if (and start end msg (stringp severity) (string= severity "fixit") (string-match "did you mean '\\(.*\\)'\\?$" msg))
        (save-excursion
          (setq insert (match-string-no-properties 1 msg))
          (rtags-goto-offset start)
          (delete-char (- end start))
          (if insert (insert insert)))))
  )

(defun rtags-fix-fixit-at-point ()
  (interactive)
  (let ((current-overlays (overlays-at (point))))
    (while (and current-overlays (not (rtags-fix-fixit-overlay (car current-overlays))))
      (setq current-overlays (cdr current-overlays))))
  )

(defvar rtags-last-update-current-project-buffer nil)
(defun rtags-update-current-project ()
  (interactive)
  (condition-case nil
      (when (and (buffer-file-name)
                 (not (eq (current-buffer) rtags-last-update-current-project-buffer)))
        (setq rtags-last-update-current-project-buffer (current-buffer))
        (let* ((rc (rtags-executable-find "rc"))
               (path (buffer-file-name))
               (arguments (list "-T" path)))
          (when rc
            (push (concat "--with-project=" path) arguments)
            (let ((mapped (if rtags-match-source-file-to-project (apply rtags-match-source-file-to-project (list path)))))
              (if (and mapped (length mapped)) (push (concat "--with-project=" mapped) arguments)))
            (apply #'start-process "rtags-update-current-project" nil rc arguments))))
    (error (message "Got error in rtags-update-current-project")))
  )

(defvar rtags-tracking-timer nil)
(defun rtags-restart-tracking-timer()
  (interactive)
  (if rtags-tracking-timer
      (cancel-timer rtags-tracking-timer))
  (setq rtags-tracking-timer
        (and rtags-tracking (string= (buffer-name) rtags-buffer-name)
             (run-with-idle-timer
              rtags-tracking-timer-interval
              nil
              (lambda ()
                (if (> (length (window-list)) 1)
                    (rtags-show-in-other-window))
                (if rtags-tracking-timer
                    (cancel-timer rtags-tracking-timer))
                (setq rtags-tracking-timer nil)))))
  )

(defun rtags-post-command-hook ()
  (interactive)
  (when rtags-enabled
    (rtags-update-current-project)
    (rtags-update-current-error)
    (rtags-restart-update-local-references-timer)
    (rtags-close-taglist)
    (rtags-restart-tracking-timer)
    (if (fboundp 'rtags-update-completions)
        (rtags-update-completions)))
  )

(add-hook 'post-command-hook (function rtags-post-command-hook))
;; (remove-hook 'post-command-hook (function rtags-post-command-hook))

(defun rtags-stop-diagnostics ()
  (interactive)
  (if (and rtags-diagnostics-process (not (eq (process-status rtags-diagnostics-process) 'exit)))
      (kill-process rtags-diagnostics-process))
  (if (get-buffer "*RTags Diagnostics*")
      (kill-buffer "*RTags Diagnostics*")))

(defun rtags-clear-diagnostics ()
  (interactive)
  (when (get-buffer "*RTags Diagnostics*")
    (let (deactivate-mark)
      (with-current-buffer "*RTags Diagnostics*"
        (setq buffer-read-only nil)
        (goto-char (point-min))
        (delete-char (- (point-max) (point-min)))
        (setq buffer-read-only t))
      )
    )
  (rtags-clear-diagnostics-overlays)
  )

(defun rtags-trim-whitespace (str)
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str)))
  str)

(defvar rtags-pending-diagnostics nil)
(defun rtags-diagnostics-process-filter (process output)
  (let ((errors)
        (oldbuffer (current-buffer))
        (proc (get-process "RTags Diagnostics"))
        (files (make-hash-table)))
    (when rtags-pending-diagnostics
      (setq output (concat rtags-pending-diagnostics output))
      (setq rtags-pending-diagnostics nil))
    (with-current-buffer (process-buffer process)
      (setq buffer-read-only nil)
      ;;   (message "matching [%s]" output)
      (let (endpos length current)
        (while (cond ((setq endpos (string-match "</checkstyle>" output))
                      (setq length 13))
                     ((setq endpos (string-match "</progress>" output))
                      (setq length 11))
                     (t nil))
          (setq current (substring output 0 (+ endpos length)))
          (setq output (rtags-trim-whitespace (substring output (+ endpos length))))
          (setq endpos (or (string-match "</checkstyle>" output)
                           (string-match "</progress>" output)))
          (rtags-reset-bookmarks)
          (rtags-parse-diagnostics (rtags-trim-whitespace current))))
      (setq buffer-read-only t)
      (when (> (length output) 0)
        (setq rtags-pending-diagnostics output)))
    )
  )

(defvar rtags-diagnostics-mode-map (make-sparse-keymap))
(define-key rtags-diagnostics-mode-map (kbd "q") 'rtags-bury-or-delete)
(define-key rtags-diagnostics-mode-map (kbd "c") 'rtags-clear-diagnostics)
(define-key rtags-diagnostics-mode-map (kbd "f") 'rtags-apply-fixit-at-point)
(set-keymap-parent rtags-diagnostics-mode-map compilation-mode-map)
(define-derived-mode rtags-diagnostics-mode compilation-mode
  (setq mode-name "rtags-diagnostics")
  (use-local-map rtags-diagnostics-mode-map)
  (if (buffer-file-name)
      (error "Set buffer with file %s read only " (buffer-file-name)))
  (setq buffer-read-only t)
  )

(defun rtags-init-diagnostics-buffer-and-process (&optional nodirty)
  (let ((buf (get-buffer-create "*RTags Diagnostics*")))
    (unless nodirty (rtags-reparse-file))
    (with-current-buffer buf
      (rtags-diagnostics-mode))
    (if (cond ((not rtags-diagnostics-process) t)
              ((eq (process-status rtags-diagnostics-process) 'exit) t)
              ((eq (process-status rtags-diagnostics-process) 'signal) t)
              (t nil))
        (let ((process-connection-type nil)) ;; use a pipe
          (setq rtags-diagnostics-process (start-process "RTags Diagnostics" buf (rtags-executable-find "rc") "-m"))
          (set-process-filter rtags-diagnostics-process (function rtags-diagnostics-process-filter))
          (rtags-clear-diagnostics))
      )
    )
  )

(defun rtags-diagnostics (&optional restart nodirty)
  (interactive "P")
  (if restart
      (rtags-stop-diagnostics))
  (rtags-init-diagnostics-buffer-and-process)
  (switch-to-buffer-other-window "*RTags Diagnostics*")
  (other-window 1)
  )

(defvar rtags-indexed nil)
(defvar rtags-file-managed nil)

(defun rtags-buffer-status (&optional buffer)
  (let ((path (buffer-file-name buffer)))
    (unless path (setq path default-directory))
    (with-temp-buffer
      (rtags-call-rc :path path "-T" path :noerror t)
      (goto-char (point-min))
      (cond ((looking-at "1") 'rtags-indexed)
            ((looking-at "2") 'rtags-file-managed)
            (t nil))))
  )

(defun rtags-is-indexed (&optional buffer)
  (equal (rtags-buffer-status buffer) 'rtags-indexed))

(defun rtags-has-filemanager (&optional buffer)
  (rtags-buffer-status buffer))

(defun rtags-handle-results-buffer (&optional noautojump nobury)
  (setq rtags-last-request-not-indexed nil)
  (let ((wasreadonly buffer-read-only))
    (setq buffer-read-only nil)
    (rtags-reset-bookmarks)
    (cond ((= (point-min) (point-max))
           (setq buffer-read-only wasreadonly)
           (message "RTags: No results") nil)
          ((and (not nobury) (= (count-lines (point-min) (point-max)) 1))
           (setq buffer-read-only wasreadonly)
           (let ((string (buffer-string)))
             (bury-buffer)
             (rtags-goto-location string)))
          (t
           (switch-to-buffer-other-window rtags-buffer-name)
           (shrink-window-if-larger-than-buffer)
           (goto-char (point-min))
           (rtags-start-mode t nil)
           (setq buffer-read-only wasreadonly)
           (when (and rtags-jump-to-first-match (not noautojump))
             (rtags-select-other-window)
             )
           )
          )
    )
  )

(defun rtags-is-definition (kind)
  (cond ((string= kind "0") nil)
        ((string= kind "f") nil)
        ((string= kind "m") nil)
        ((string= kind "c") nil)
        ((string= kind "s") nil)
        ((string= kind "r") nil)
        (t t)))

;; (defun rtags-compare-int (l r)
;;   (let ((lint (string-to-number l))
;;         (rint (string-to-number r)))
;;     (cond ((< lint rint) -1)
;;           ((> lint rint) 1)
;;           (t 0))
;;     )
;;   )

(defun rtags-async-rc-filter (process output)
  (let ((buf (process-buffer process))
        (wasempty)
        (hasline))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (save-excursion
        (setq wasempty (= (point-min) (point-max)))
        (goto-char (point-max))
        (insert output)
        (goto-char (point-min))
        (when (re-search-forward "\n" nil t)
          (setq hasline t)))
      (setq buffer-read-only t)
      (when (and wasempty hasline)
        (when (buffer-file-name)
          (error "Set buffer with file %s read only " (buffer-file-name)))
        (rtags-handle-results-buffer nil t))))
  )

(defun rtags-async-rc-sentinel (process state)
  (when (string= state "finished\n")
    (let ((buf (process-buffer process)))
      (when buf
        (with-current-buffer buf
          (let ((wasreadonly buffer-read-only))
            (setq buffer-read-only nil)
            (flush-lines "^[ \t]*$" (point-min) (point-max) nil)
            (flush-lines "Process finished rc" (point-min) (point-max) nil)
            (setq buffer-read-only wasreadonly)))))
    )
  )

(defun rtags-standard-save-hook ()
  (interactive)
  (if (and (get-buffer "*RTags Diagnostics*") (rtags-is-indexed))
      (rtags-clear-diagnostics))
  t)

(defun rtags-filename-complete (string predicate code)
  (let ((complete-list (make-vector 63 0)))
    (if (or (string-match "\\(.*\\),[0-9]+" string)
            (string-match "\\(.*\\):[0-9]+:[0-9]+" string)
            (string-match "\\(.*\\):[0-9]+" string))
        (setq string (match-string-no-properties 1 string)))
    (with-temp-buffer
      (rtags-call-rc :path default-directory "-P" string (if rtags-find-file-case-insensitive "-I"))
      (goto-char (point-min))
      (if (equal "" string)
          (while (not (eobp))
            (intern (buffer-substring-no-properties (point-at-bol) (point-at-eol)) complete-list)
            (forward-line))
        (let ((match-string-no-properties (format  ".*\\(%s.*\\)" string)))
          (while (not (eobp))
            (if (looking-at match-string-no-properties)
                (intern (buffer-substring-no-properties (match-beginning 1) (match-end 1)) complete-list))
            (forward-line))))
      (cond ((eq code nil)
             (try-completion string complete-list predicate))
            ((eq code t)
             (all-completions string complete-list predicate))
            ((eq code 'lambda)
             (if (intern-soft string complete-list) t nil))))))

(defvar rtags-taglist-protected nil)
(defvar rtags-taglist-locations nil)
(define-derived-mode rtags-taglist-mode fundamental-mode
  (setq mode-name "rtags-taglist")
  (use-local-map rtags-mode-map)
  (run-hooks 'rtags-taglist-mode-hook)
  )

(defun rtags-close-taglist ()
  (interactive)
  (unless rtags-taglist-protected
    (let ((buf (get-buffer rtags-buffer-name)))
      (if (and buf
               (not (eq (current-buffer) buf))
               (eq (with-current-buffer buf major-mode) 'rtags-taglist-mode))
          (let ((windows (window-list)))
            (while windows
              (when (eq (window-buffer (car windows)) buf)
                (delete-window (car windows))
                (setq windows nil))
              (setq windows (cdr windows)))
            )
        )
      )
    )
  )

;; category (list (text . (location . linenumber)))
(defun rtags-taglist-insert-category (category name)
  (let ((max 0))
    (when category
      (set-mark-command nil)
      (insert name ":\n")
      (facemenu-set-underline)
      (while category
        (add-to-list 'rtags-taglist-locations (cons (line-number-at-pos) (cdar category)))
        (let* ((text (caar category))
               (len (length text)))
          (insert " " text "\n")
          (setq max (max len max)))
        (setq category (cdr category)))
      )
    max)
  )

(defun rtags-taglist ()
  (interactive)
  (rtags-save-location)
  (setq rtags-taglist-locations nil)
  (let* ((fn (buffer-file-name)) functions classes variables enums macros other)
    (with-temp-buffer
      (rtags-call-rc :path fn :path-filter fn "-F" "--cursor-kind" "--display-name" "--no-context")
      ;; (message (buffer-string))
      (unless (= (point-min) (point-max))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
            (if (string-match "^\\(.*:\\)\\([0-9]+\\)\\(:[0-9]+:\\)\t\\(.*\\)\t\\(.*\\)$" line)
                (let ((loc-start (match-string-no-properties 1 line))
                      (linenum (match-string-no-properties 2 line))
                      (loc-end (match-string-no-properties 3 line))
                      (text (match-string-no-properties 4 line))
                      (type (match-string-no-properties 5 line)))
                  (add-to-list (cond ((or (string= type "FunctionDecl") (string= type "CXXMethod")
                                          (string= type "CXXConstructor") (string= type "CXXDestructor")) 'functions)
                                     ((or (string= type "ClassDecl") (string= type "StructDecl")) 'classes)
                                     ((or (string= type "VarDecl") (string= type "FieldDecl") (string= type "ParmDecl")) 'variables)
                                     ((or (string= type "EnumDecl") (string= type "EnumConstantDecl")) 'enums)
                                     ((or (string= type "macro definition") (string= type "include directive")) 'macros)
                                     (t 'other))
                               (cons (concat text ":" linenum) (concat loc-start linenum loc-end))))))
          (forward-line))
        )
      )
    (when (or functions classes variables enums macros other)
      (delete-other-windows)
      (let ((buf (rtags-get-buffer)) (max 0))
        (with-current-buffer buf
          (erase-buffer)
          (setq max (max max (rtags-taglist-insert-category functions "Functions")))
          (setq max (max max (rtags-taglist-insert-category classes "Classes/Structs")))
          (setq max (max max (rtags-taglist-insert-category variables "Vars/Fields/Params")))
          (setq max (max max (rtags-taglist-insert-category enums "Enums")))
          (setq max (max max (rtags-taglist-insert-category macros "Macros")))
          (setq max (max max (rtags-taglist-insert-category other "Other")))
          (setq buffer-read-only t)
          (goto-char (point-min))
          (forward-line))
        (split-window-horizontally (min (/ (frame-width) 2) (+ 2 max)))
        (switch-to-buffer buf)
        (rtags-taglist-mode)
        )
      )
    )
  )

(defun rtags-select (&optional other-window remove show)
  (interactive "P")
  (let* ((line (line-number-at-pos))
         (bookmark (format "R_%d" line))
         (window (selected-window)))
    (cond ((eq major-mode 'rtags-taglist-mode)
           (rtags-goto-location (cdr (assoc line rtags-taglist-locations)) nil other-window))
          ((and (>= rtags-buffer-bookmarks line)
                (member bookmark (bookmark-all-names)))
           (when other-window
             (if (= (length (window-list)) 1)
                 (split-window))
             (other-window 1))
           (bookmark-jump bookmark)
           (rtags-location-stack-push))
          (t (rtags-goto-location (buffer-substring-no-properties (point-at-bol) (point-at-eol)) nil other-window)))
    (if remove
        (delete-window window)
      (if show
          (select-window window)))
    )
  )

(defun rtags-select-other-window (&optional not-other-window)
  (interactive "P")
  (rtags-select (not not-other-window)))

(defun rtags-show-in-other-window ()
  (interactive)
  ;; (message "About to show")
  (rtags-select t nil t))

(defun rtags-select-and-remove-rtags-buffer ()
  (interactive)
  (rtags-select t t))

(defun rtags-imenu ()
  (interactive)
  (rtags-save-location)
  (let* ((fn (buffer-file-name))
         (alternatives (with-temp-buffer
                         (rtags-call-rc :path fn :path-filter fn "--imenu" "--list-symbols" "-Y")
                         (eval (read (buffer-string)))))
         (match (car alternatives)))
    (if (> (length alternatives) 1)
        (setq match (ido-completing-read "Symbol: " alternatives)))
    (if match
        (rtags-goto-location (with-temp-buffer (rtags-call-rc :path fn "-F" match :path-filter fn) (buffer-string)))
      (message "RTags: No symbols"))))

(defun rtags-append (txt)
  (goto-char (point-min))
  (while (< (point-at-eol) (point-max))
    (goto-char (point-at-eol))
    (insert txt)
    (forward-line))
  )

(defvar rtags-find-file-history nil)
(defun rtags-find-file (&optional prefix tagname)
  (interactive "P")
  (rtags-save-location)
  (let ((tagname (rtags-current-symbol t)) prompt input offset line column
        (prefer-exact rtags-find-file-prefer-exact-match))
    (if prefix
        (setq prefer-exact (not prefer-exact)))
    (if (> (length tagname) 0)
        (setq prompt (concat (format "Find rfiles (default %s): " tagname)))
      (setq prompt "Find rfiles: "))
    (rtags-is-indexed)
    (setq input (completing-read prompt (function rtags-filename-complete) nil nil nil 'rtags-find-file-history))
    (setq rtags-find-file-history (remove-duplicates rtags-find-file-history :from-end t :test 'equal))
    (cond ((string-match "\\(.*\\),\\([0-9]+\\)" input)
           (progn
             (setq tagname (match-string-no-properties 1 input))
             (setq offset (string-to-number (match-string-no-properties 2 input)))))
          ((string-match "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)" input)
           (progn
             (setq tagname (match-string-no-properties 1 input))
             (setq line (string-to-number (match-string-no-properties 2 input)))
             (setq column (string-to-number (match-string-no-properties 3 input)))))
          ((string-match "\\(.*\\):\\([0-9]+\\)" input)
           (setq tagname (match-string-no-properties 1 input))
           (setq line (string-to-number (match-string-no-properties 2 input))))
          ((not (equal "" input))
           (setq tagname input))
          (t nil))

    ;; (message (format "%s %s %d" input tagname rtags-find-file-offset))
    (rtags-reset-bookmarks)
    (rtags-location-stack-push)

    (with-current-buffer (rtags-get-buffer)
      (rtags-call-rc "-K" "-P" tagname
                     (if rtags-find-file-case-insensitive "-I")
                     (if prefer-exact "-A"))
      (and (= (point-min) (point-max))
           (string-match "[^/]\\.\\.[^/]" tagname)
           (rtags-call-rc "-K" "-P"
                          (replace-regexp-in-string "\\([^/]\\)\\.\\.\\([^/]\\)" "\\1.\\2" tagname)
                          (if rtags-find-file-case-insensitive "-I")
                          (if prefer-exact "-A")))

      (cond (offset (rtags-append (format ",%d" offset)))
            ((and line column) (rtags-append (format ":%d:%d" line column)))
            ((and line) (rtags-append (format ":%d" line)))
            (t nil))
      ;; (message (format "Got lines and shit %d\n[%s]" (count-lines (point-min) (point-max)) (buffer-string)))
      (goto-char (point-min))
      (cond ((= (point-min) (point-max)) t)
            ((= (count-lines (point-min) (point-max)) 1) (rtags-goto-location (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
            (t (progn
                 (switch-to-buffer-other-window rtags-buffer-name)
                 (shrink-window-if-larger-than-buffer)
                 (rtags-start-mode nil t)
                 )))
      )
    )
  )

(defun rtags-show-rtags-buffer ()
  (interactive)
  (if (get-buffer rtags-buffer-name)
      (display-buffer rtags-buffer-name)))

(defun rtags-fixit (&optional ediff buffer)
  (interactive "P")
  (save-some-buffers)
  (unless buffer
    (setq buffer (current-buffer)))
  (save-excursion
    (let* ((path (buffer-file-name buffer))
           (tempbuf nil)
           (buffertext (if ediff (with-current-buffer buffer (buffer-string))))
           (min (line-number-at-pos (if mark-active (region-beginning) (point-min))))
           (max (line-number-at-pos (if mark-active (region-end) (point-max))))
           (line nil))
      (with-temp-buffer
        (rtags-call-rc :path path "--fixit" path)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
            (if (string-match "^\\([0-9]+\\):\\([0-9]+\\) \\([0-9]+\\) \\(.*\\)$" line)
                (let ((line (string-to-number (match-string-no-properties 1 line)))
                      (col (string-to-number (match-string-no-properties 2 line)))
                      (length (string-to-number (match-string-no-properties 3 line)))
                      (text (match-string-no-properties 4 line)))
                  (when (and (>= line min) (<= line max))
                    (when (not (or (not ediff) tempbuf))
                      (setq tempbuf (rtags-get-buffer (format "*RTags Fixit - %s *" path)))
                      (with-current-buffer tempbuf
                        (insert buffertext)))
                    (with-current-buffer (or tempbuf buffer)
                      (rtags-goto-line-col line col)
                      (delete-char length) ;; may be 0
                      (insert text))))))
          ;; (message (format "got something %d to %d => [%s]" start end text))))
          (forward-line))
        )
      (if tempbuf
          (let ((tempbufname (format "/tmp/rtags-fixit-%s" (file-name-nondirectory path))))
            (with-current-buffer tempbuf (write-file tempbufname))
            (kill-buffer tempbuf)
            (ediff path tempbufname)))
      )
    )
  )

(defun rtags-current-symbol-name (&optional cursorinfo)
  (unless cursorinfo
    (setq cursorinfo (rtags-cursorinfo)))
  (let ((container (string-match "^Container:" cursorinfo))
        (symbolname (string-match "^SymbolName: \\(.*\\)$" cursorinfo)))
    (if (and symbolname (or (not container) (< symbolname container)))
        (match-string-no-properties 1 cursorinfo))
    )
  )

(defun rtags-current-container-name (&optional cursorinfo)
  (unless cursorinfo
    (setq cursorinfo (rtags-cursorinfo)))
  (let* ((container (string-match "^Container:" cursorinfo))
         (symbolname (string-match "^SymbolName: \\(.*\\)$" cursorinfo (if container container 0))))
    (if container
        (match-string-no-properties 1 cursorinfo)
      nil)))

(defun rtags-cursor-extent (&optional location)
  (let ((cursorinfo (rtags-cursorinfo location)))
    (if (string-match "^Range: \\([0-9]+\\)-\\([0-9]+\\)$" cursorinfo)
        (let ((start (+ (string-to-number (match-string-no-properties 2 cursorinfo)) 1))
              (end (+ (string-to-number (match-string-no-properties 3 cursorinfo)) 1)))
          (cons start end)))))

(defvar rtags-other-window-window nil)
(defun rtags-remove-other-window ()
  (interactive)
  (let ((ret ""))
    (if (and (> (length (window-list nil nil)) 1)
             rtags-other-window-window
             (window-live-p rtags-other-window-window))
        (progn
          (select-window rtags-other-window-window)
          (setq ret (rtags-current-location))
          (delete-window rtags-other-window-window)
          (setq rtags-other-window-window nil)))
    ret))

(defcustom rtags-timeout nil
  "Max amount of ms to wait for operation to finish"
  :group 'rtags
  :type 'integer)

(defcustom rtags-find-file-case-insensitive nil
  "Treat files case insensitively"
  :group 'rtags
  :type 'boolean)

(defcustom rtags-find-file-prefer-exact-match t
  "Jump directly to files that exactly match the filename for rtags-find-file"
  :group 'rtags
  :type 'boolean)

(defcustom rtags-match-source-file-to-project nil
  "Function to match source file to a build directory"
  :group 'rtags
  :type 'function)

(defcustom rtags-other-window-window-size-percentage 30 "Percentage size of other buffer" :group 'rtags :type 'integer)
(defun rtags-show-target-in-other-window ()
  (interactive)
  (let ((target (rtags-target)))
    (unless target
      (let ((token (rtags-current-token)))
        (if token
            (with-temp-buffer
              (rtags-call-rc "--declaration-only" "-N" "-F" token)
              (if (= (count-lines (point-min) (point-max)) 1)
                  (setq target (buffer-substring-no-properties (point) (- (point-max) 1))))))))
    (if target
        (let ((other-window-content (rtags-remove-other-window))
              (win (selected-window))
              (height (* (window-height) (- 100 rtags-other-window-window-size-percentage))))
          (unless (string= target other-window-content)
            (progn
              (setq height (/ height 100))
              (setq rtags-other-window-window (split-window nil height))
              (select-window rtags-other-window-window)
              (rtags-goto-location target)
              (recenter-top-bottom 0)
              (select-window win)))))))

(defvar rtags-local-references-overlays nil)
(defun rtags-clear-local-references-overlays()
  (interactive)
  (while rtags-local-references-overlays
    (delete-overlay (car rtags-local-references-overlays))
    (setq rtags-local-references-overlays (cdr rtags-local-references-overlays)))
  )

(defun rtags-offset-for-line-column (line col)
  (let (deactivate-mark)
    (save-excursion
      (rtags-goto-line-col line col)
      (rtags-offset)))
  )

(defun rtags-range-visible (start end)
  (and (>= start (window-start))
       (<= start (window-end))
       (<= end (window-end))))

(defvar rtags-cached-local-references nil)
(defun rtags-update-local-references ()
  (interactive)
  (let ((path (buffer-file-name))
        (loc (rtags-current-location))
        (symlen (length (rtags-current-token)))
        (start (window-start))
        (end (window-end))
        (lines nil))
    (if (= symlen 0)
        (setq symlen 1))
    (when (not (string= loc rtags-cached-local-references))
      (setq rtags-cached-local-references loc)
      (rtags-clear-local-references-overlays)
      (with-temp-buffer
        (rtags-call-rc :path path "-r" loc "-e" "-N" :path-filter path)
        (setq lines (split-string (buffer-string) "\n" t)))
      (while lines
        (let ((cur (car lines))
              (offset nil))
          (when (string-match "\\(.*\\),\\([0-9]+\\)" cur)
            ;; (message "foobar |%s|%s|" (match-string-no-properties 1 cur) (match-string-no-properties 2 cur))
            (setq offset (1+ (string-to-number (match-string-no-properties 2 cur))))
            (cond ((> offset end) (setq lines nil))
                  ((< (+ offset symlen) start))
                  (t
                   (let ((overlay (make-overlay offset (+ offset symlen) nil t)))
                     (overlay-put overlay 'face 'rtags-local-reference)
                     (setq rtags-local-references-overlays (append rtags-local-references-overlays (list overlay))))))))
        (setq lines (cdr lines)))
      )
    )
  )


(defvar rtags-local-references-timer nil)
(defun rtags-restart-update-local-references-timer ()
  (interactive)
  (if rtags-local-references-timer
      (cancel-timer rtags-local-references-timer))
  (setq rtags-local-references-timer
        (and rtags-local-references-enabled
             (or (eq major-mode 'c++-mode)
                 (eq major-mode 'c-mode))
             (not (string= rtags-cached-local-references (rtags-current-location)))
             (progn
               (rtags-clear-local-references-overlays)
               (run-with-idle-timer rtags-local-references-timer-interval
                                    nil (function rtags-update-local-references))))
        )
  )

(defun rtags-toggle-file-suspended()
  (interactive)
  (let ((buffer (buffer-file-name)))
    (if buffer
        (with-temp-buffer
          (rtags-call-rc :path buffer "-X" buffer)
          (if (> (point-max) (point-min))
              (message (buffer-substring-no-properties (point-min) (1- (point-max))))
            (message (buffer-string))))))
  )

(defun rtags-clear-suspended-files()
  (interactive)
  (let ((buffer (buffer-file-name)))
    (if buffer
        (with-temp-buffer
          (rtags-call-rc :path buffer "-X" "clear")
          (if (> (point-max) (point-min))
              (message (buffer-substring-no-properties (point-min) (1- (point-max))))
            (message (buffer-string))))))
  )

(defun rtags-list-suspended-files()
  (interactive)
  (let ((buffer (buffer-file-name)))
    (if buffer
        (with-temp-buffer
          (rtags-call-rc :path buffer "-X")
          (if (> (point-max) (point-min))
              (message (buffer-substring-no-properties (point-min) (1- (point-max))))
            (message (buffer-string))))))
  )


(provide 'rtags)
