(defgroup rtags nil
  "Minor mode for rtags."
  :group 'tools
  :prefix "rtags-")

(require 'cl)
(require 'ido)
(require 'dabbrev)
(require 'cc-mode)
(require 'dash)
(require 's)
(require 'bookmark)
(require 'compile)

(defvar rtags-last-buffer nil)
(defvar rtags-path-filter nil)
(defvar rtags-path-filter-regex nil)
(defvar rtags-range-filter nil)
(defvar rtags-mode-hook nil)
(defvar rtags-no-otherbuffer nil)
(defface rtags-path nil "Path" :group 'rtags)
(defface rtags-context nil "Context" :group 'rtags)
(defvar rtags-path-face 'rtags-path "Path part")
(defvar rtags-context-face 'rtags-context "Context part")
(defconst rtags-buffer-name "*RTags*")
(defvar rtags-completion nil)
(defvar rtags-completion-cache-file-name "")
(defvar rtags-completion-cache-line 0)
(defvar rtags-completion-cache-column 0)
(defvar rtags-completion-cache-line-contents "")
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
(define-key rtags-mode-map (kbd "RET") 'rtags-select-other-buffer)
(define-key rtags-mode-map (kbd "M-RET") 'rtags-select-and-remove-rtags-buffer)
(define-key rtags-mode-map (kbd "ENTER") 'rtags-select-other-buffer)
(define-key rtags-mode-map (kbd "SPC") 'rtags-select)
(define-key rtags-mode-map (kbd "q") 'rtags-bury-or-delete)
(define-key rtags-mode-map (kbd "j") 'next-line)
(define-key rtags-mode-map (kbd "k") 'previous-line)

(define-derived-mode rtags-mode fundamental-mode
  (setq buffer-read-only nil)
  (set (make-local-variable 'font-lock-defaults) '(rtags-font-lock-keywords))
  (setq mode-name "rtags")
  (use-local-map rtags-mode-map)
  (run-hooks 'rtags-mode-hook)
  (setq buffer-read-only t)
  (goto-char (point-min))
  )

(defun rtags-start-mode (bookmarks)
  (rtags-reset-bookmarks)
  (if bookmarks
    (let ((buf (current-buffer)))
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)")
            (let ((file (match-string 1))
                  (line (string-to-int (match-string 2)))
                  (column (string-to-int (match-string 3))))
              (let (deactivate-mark)
                (save-excursion
                  (set-buffer (find-file-noselect file))
                  (goto-line line)
                  (beginning-of-line)
                  (forward-char (- column 1))
                  (setq rtags-buffer-bookmarks (+ rtags-buffer-bookmarks 1))
                  (bookmark-set (format "R_%d" rtags-buffer-bookmarks))
                  (set-buffer buf)))))
        (next-line))
      ))
  (if (= (point-at-bol) (point-max))
      (delete-char -1))
  (rtags-mode))

(defun rtags-reset-bookmarks ()
  (while (> rtags-buffer-bookmarks 0)
    (bookmark-delete (format "R_%d" rtags-buffer-bookmarks))
    (setq rtags-buffer-bookmarks (- rtags-buffer-bookmarks 1)))
  )

(defun rtags-next-match () (interactive) (rtags-next-prev-match t))
(defun rtags-previous-match () (interactive) (rtags-next-prev-match nil))

(defun rtags-next-prev-match (next)
  (if (get-buffer rtags-buffer-name)
      (let (target
            (win (get-buffer-window rtags-buffer-name)))
        (if win (select-window win))
        (set-buffer rtags-buffer-name)
        (when (> (count-lines (point-max) (point-min)) 1)
          (cond ((and (= (point-at-bol) (point-min)) (not next))
                 (setq target (point-max))
                 (message "*RTags* Wrapped"))
                ((and (= (point-at-eol) (point-max)) next)
                 (setq target (point-min))
                 (message "*RTags* Wrapped"))
                (next
                 (setq target (point-at-bol 2)))
                (t
                 (setq target (point-at-bol 0))))
          (goto-char target)
          (beginning-of-line)
          (if win (rtags-select-other-buffer) (rtags-select))))))

(defun rtags-executable-find (exe)
  (let ((result (if rtags-path (concat rtags-path "/bin/" exe) (executable-find exe))))
    (if (and result (file-exists-p result))
        result)))


(defun rtags-call-rc (path &rest arguments)
  (apply #'rtags-call-rc-helper path nil t arguments))

(defun rtags-call-rc-unsaved (path unsaved-pos output &rest arguments)
  (apply #'rtags-call-rc-helper path unsaved-pos output arguments))

(defun rtags-call-rc-helper (path unsaved-pos output &rest arguments)
  (save-excursion
    (let ((rc (rtags-executable-find "rc")))
      (when rc
        (setq arguments (remove-if '(lambda (arg) (not arg))
                                   arguments))
        (if rc
            (progn
              (if rtags-autostart-rdm
                  (push (if rtags-rdm-log-enabled "--autostart-rdm=-L/tmp/rdm.log" "--autostart-rdm") arguments))
              (if rtags-path-filter
                  (progn
                    (push (format "--path-filter=%s" rtags-path-filter) arguments)
                    (if rtags-path-filter-regex
                        (push "-Z" arguments))))
              (if rtags-range-filter
                  (push rtags-range-filter arguments))

              (if rtags-timeout
                  (push (format "--timeout=%d" rtags-timeout) arguments))
              (if path
                  (progn
                    (if rtags-match-source-file-to-project
                        (let ((mapped (if rtags-match-source-file-to-project (apply rtags-match-source-file-to-project (list path)))))
                          (if (and mapped (length mapped)) (push (concat "--with-project=" mapped) arguments))))
                    (push (concat "--with-project=" path) arguments)))

              (rtags-log (concat rc " " (combine-and-quote-strings arguments)))
              (if unsaved-pos
                  (apply #'call-process-region (point-min) unsaved-pos rc nil (list output t) nil arguments)
                (apply #'call-process rc nil (list output nil) nil arguments))
              (goto-char (point-min))
              (rtags-log (buffer-string))
              (when (looking-at "Can't seem to connect to server")
                (message "Can't seem to connect to server. Is rdm running?")
                (erase-buffer))
              (> (point-max) (point-min))))))))

(defun rtags-path-for-project (&optional buffer)
  (expand-file-name (if (buffer-file-name buffer)
                        (buffer-file-name buffer)
                      default-directory)))

(defvar rtags-preprocess-keymap (make-sparse-keymap))
(define-key rtags-preprocess-keymap (kbd "q") 'rtags-bury-or-delete)
(set-keymap-parent rtags-preprocess-keymap c++-mode-map)
(define-derived-mode rtags-preprocess-mode c++-mode
  (setq mode-name "rtags-preprocess")
  (use-local-map rtags-diagnostics-mode-map)
  (setq buffer-read-only t)
  )

(defun rtags-builds (file)
  (with-temp-buffer
    (rtags-call-rc nil "--builds" file)
    (buffer-string))
  )

(defun rtags-preprocess-file (&optional build-index buffer)
  (interactive "P")
  (setq build-index (cond ((and build-index (not (integerp build-index)))
                           (read-from-minibuffer "Build index: " nil nil nil 0)) ;; should insert builds
                          ((not build-index) 0)
                          (t build-index)))
  (let ((fn (format "%s_%d" (buffer-file-name buffer) build-index))
        bufname narrow-start narrow-end)
    (if (and mark-active
             (not (equal (region-beginning) (region-end))))
        (setq narrow-start (+ 1 (count-lines (point-min) (region-beginning)))
              narrow-end (+ 1 (count-lines (point-min) (region-end)))))
    (if fn
        (let ((preprocess-buffer (rtags-get-buffer (format "*RTags preprocessed %s*" fn))))
          (rtags-location-stack-push)
          (with-current-buffer preprocess-buffer
            (rtags-call-rc nil "--preprocess" fn)
            (if (and narrow-start narrow-end)
                (let ((match-regexp (concat "^# \\([0-9]*\\) \"" (file-truename fn) "\""))
                      last-match last-line start end)
                  (while (re-search-forward match-regexp nil t)
                    (let ((current-line (string-to-int (match-string 1))))
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
      )))

(defun rtags-reparse-file (&optional buffer)
  (interactive)
  (let ((path (rtags-path-for-project))
        (file (buffer-name buffer)))
    (with-temp-buffer
      (rtags-call-rc path "-V" file))
    (message (format "Dirtied %s" file))
    )
  )

;; /home/abakken/dev (loaded) <=

(defun rtags-set-current-project ()
  (interactive)
  (let ((projects nil)
        (project nil)
        (current ""))
    (with-temp-buffer
      (rtags-call-rc nil "-w")
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring (point-at-bol) (point-at-eol))))
          (if (string-match "^\\([^ ]+\\)[^<]*<=$" line)
              (let ((name (match-string 1 line)))
                (setq projects (add-to-list 'projects name t))
                (setq current name))
            (if (string-match "^\\([^ ]+\\)[^<]*$" line)
                (setq projects (add-to-list 'projects (match-string 1 line))))))
        (next-line))
      )
    (setq project (ido-completing-read
                   (format "RTags select project (current is %s): " current)
                   projects))
    (if project
        (with-temp-buffer (rtags-call-rc nil "-w" project)))
    )
  )
;; (message (format "we picked %s" project))

;; (message (combine-and-quote-strings projects))
;; (while (looking-at "^\\([^ ]*\\)")
;; (message (format "%s %s" (match-string 1) (match-string 2)))
;; )
;;   )
;; )

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
  (save-excursion
    (let ((name (if no-symbol-name nil (rtags-current-symbol-name))))
      (unless name
        (cond
         ((looking-at "[0-9A-Za-z_]")
          (while (and (not (bolp)) (looking-at "[0-9A-Za-z_]"))
            (forward-char -1))
          (if (not (looking-at "[0-9A-Za-z_]")) (forward-char 1)))
         (t
          (while (looking-at "[ \t]")
            (forward-char 1))))
        (if (looking-at "[A-Za-z_][A-Za-z_0-9]*")
            (setq name (buffer-substring (match-beginning 0) (match-end 0)))))
      name)))

(defun rtags-cursorinfo (&optional location verbose)
  (let ((loc (or location (rtags-current-location)))
        (path (rtags-path-for-project)))
    (with-temp-buffer
      (rtags-call-rc path
                     "-U"
                     loc
                     (if verbose "--cursorinfo-include-parents")
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
    (rtags-call-rc (rtags-path-for-project) "--dependencies" fn)
    (rtags-start-mode nil)))

(defun rtags-print-enum-value-at-point (&optional location)
  (interactive)
  (let ((info (rtags-cursorinfo location)))
    (cond ((string-match "^Enum Value: \\([0-9]+\\) *$" info)
           (let ((enumval (match-string 1 info)))
             (message "%s - %s - 0x%X" (rtags-current-symbol-name info) enumval (string-to-int enumval))))
          ((string-match "^Type: Enum *$" info)
           (let ((target (rtags-target)))
             (when target
               (setq info (rtags-cursorinfo target))
               (if (string-match "^Enum Value: \\([0-9]+\\) *$" info)
                   (let ((enumval (match-string 1 info)))
                     (message "%s - %s - 0x%X" (rtags-current-symbol-name info) enumval (string-to-int enumval)))))))
          (t (message "RTags: No enum here") nil))))

(defun rtags-offset (&optional p)
  (save-excursion
    (if p
        (goto-char p)
      (let ((prev (buffer-local-value enable-multibyte-characters (current-buffer)))
            (loc (local-variable-p enable-multibyte-characters))
            (pos))
        (set-buffer-multibyte nil)
        (setq pos (- (point) 1))
        (set-buffer-multibyte prev)
        (unless loc
          (kill-local-variable enable-multibyte-characters))
        pos))))

(defun rtags-goto-offset (pos)
  (interactive "NOffset: ")
  (let ((prev (buffer-local-value enable-multibyte-characters (current-buffer)))
        (loc (local-variable-p enable-multibyte-characters)))
    (set-buffer-multibyte nil)
    (goto-char (+ pos 1))
    (set-buffer-multibyte prev)
    (unless loc
      (kill-local-variable enable-multibyte-characters))))

(defun rtags-current-location ()
  (format "%s,%d" (buffer-file-name) (rtags-offset)))

(defun rtags-log (log)
  (if rtags-rc-log-enabled
      (save-excursion
        (set-buffer (get-buffer-create "*RTags Log*"))
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

(defun rtags-goto-location (location &optional nobookmark otherbuffer)
  "Go to a location passed in. It can be either: file,12 or file:13:14 or plain file"
  ;;  (message (format "rtags-goto-location \"%s\"" location))
  (when (> (length location) 0)
    (if rtags-no-otherbuffer (setq otherbuffer nil))
    (cond ((string-match "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)" location)
           (let ((line (string-to-int (match-string 2 location)))
                 (column (string-to-int (match-string 3 location))))
             (if otherbuffer
                 (find-file-other-window (match-string 1 location))
               (find-file (match-string 1 location)))
             (run-hooks rtags-after-find-file-hook)
             (goto-line line)
             (beginning-of-line)
             (forward-char (- column 1))
             t))
          ((string-match "\\(.*\\):\\([0-9]+\\)" location)
           (let ((line (string-to-int (match-string 2 location))))
             (if otherbuffer
                 (find-file-other-window (match-string 1 location))
               (find-file (match-string 1 location)))
             (run-hooks rtags-after-find-file-hook)
             (goto-line line)
             t))
          ((string-match "\\(.*\\),\\([0-9]+\\)" location)
           (let ((offset (string-to-int (match-string 2 location))))
             (if otherbuffer
                 (find-file-other-window (match-string 1 location))
               (find-file (match-string 1 location)))
             (run-hooks rtags-after-find-file-hook)
             (rtags-goto-offset offset)
             t))
          (t
           (if (string-match "^ +\\(.*\\)$" location)
               (setq location (match-string 1 location)))
           (if otherbuffer
               (find-file-other-window location)
             (find-file location))
           )
          )
    (unless nobookmark (rtags-location-stack-push))
    )
  )

(defun rtags-find-symbols-by-name-internal (p references filter)
  (rtags-save-location)
  (rtags-setup-filters filter)
  (let ((tagname (if mark-active
                     (buffer-substring (region-beginning) (region-end))
                   (rtags-current-symbol)))
        (switch (if references "-R" "-F"))
        (path (rtags-path-for-project))
        prompt
        input)
    (if tagname
        (setq prompt (concat p ": (default " tagname ") "))
      (setq prompt (concat p ": ")))
    (setq input (completing-read prompt (function rtags-symbolname-complete) nil nil nil 'rtags-symbol-history))
    (setq rtags-symbol-history (remove-duplicates rtags-symbol-history :from-end t :test 'equal))
    (if (not (equal "" input))
        (setq tagname input))
    (with-current-buffer (rtags-get-buffer)
      (rtags-call-rc path switch tagname "-l")
      (rtags-reset-bookmarks)
      (rtags-handle-completion-buffer)
      )
    )
  (rtags-setup-filters nil)
  )

(defun rtags-remove-completion-buffer ()
  (interactive)
  (kill-buffer (current-buffer))
  (switch-to-buffer rtags-last-buffer))

(defun rtags-symbolname-completion-get (string)
  (with-temp-buffer
    (rtags-call-rc nil "-Y" "-S" string)
    (eval (read (buffer-string)))))

(defun rtags-symbolname-completion-exactmatch (string)
  (with-temp-buffer
    (rtags-call-rc nil "-N" "-F" string)
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

(defcustom rtags-error-timer-interval .5
  "Interval for minibuffer error timer"
  :group 'rtags
  :type 'number)

(defcustom rtags-completion-enabled nil
  "Whether rtags completion is enabled"
  :group 'rtags
  :type 'boolean)

(defcustom rtags-completion-timer-interval .1
  "Interval for completion timer"
  :group 'rtags
  :type 'number)

(defcustom rtags-cursorinfo-timer-enabled nil
  "Whether rtags cursorinfo is enabled"
  :group 'rtags
  :type 'boolean)

(defcustom rtags-cursorinfo-timer-interval 1
  "Interval for cursorinfo timer"
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

(defcustom rtags-rdm-log-enabled nil
  "If t, log for autostarted rdm"
  :group 'rtags
  :type 'boolean)

(defcustom rtags-autostart-rdm nil
  "If autostart rdm"
  :group 'rtags
  :type 'boolean)


(defun rtags-enable-standard-keybindings (&optional map)
  (interactive)
  (unless map
    (setq map c-mode-base-map))
  (define-key map (kbd "C-x r .") (function rtags-find-symbol-at-point))
  (define-key map (kbd "C-x r ,") (function rtags-find-references-at-point))
  (define-key map (kbd "C-x r v") (function rtags-find-virtuals-at-point))
  (define-key map (kbd "C-x r V") (function rtags-print-enum-value-at-point))
  (define-key map (kbd "C-x r /") (function rtags-find-all-references-at-point))
  (define-key map (kbd "C-x r >") (function rtags-find-symbol))
  (define-key map (kbd "C-x r <") (function rtags-find-references))
  (define-key map (kbd "C-x r [") (function rtags-location-stack-back))
  (define-key map (kbd "C-x r ]") (function rtags-location-stack-forward))
  (define-key map (kbd "C-x r C") (function rtags-switch-to-completion-buffer))
  (define-key map (kbd "C-x r D") (function rtags-diagnostics))
  (define-key map (kbd "C-x r G") (function rtags-guess-function-at-point))
  (define-key map (kbd "C-x r p") (function rtags-set-current-project))
  (define-key map (kbd "C-x r P") (function rtags-print-dependencies))
  (define-key map (kbd "C-x r e") (function rtags-reparse-file))
  (define-key map (kbd "C-x r E") (function rtags-preprocess-file))
  (define-key map (kbd "C-x r R") (function rtags-rename-symbol))
  (define-key map (kbd "C-x r U") (function rtags-print-cursorinfo))
  (define-key map (kbd "C-x r O") (function rtags-goto-offset))
  (define-key map (kbd "C-x r ;") (function rtags-find-file))
  (define-key map (kbd "C-x r F") (function rtags-fixit))
  (define-key map (kbd "C-x r x") (function rtags-fix-fixit-at-point))
  (define-key map (kbd "C-x r B") (function rtags-show-rtags-buffer))
  (define-key map (kbd "C-x r I") (function rtags-imenu))
  )

(defun rtags-print-current-location ()
  (interactive)
  (message (rtags-current-location)))

(defun rtags-quit-rdm () (interactive)
  (call-process (rtags-executable-find "rc") nil nil nil "--quit-rdm"))

(defun rtags-switch-to-completion-buffer () (interactive)
  (let ((buf (get-buffer "*RTags Completions*")))
    (switch-to-buffer-other-window buf)))

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

(defun rtags-target (&optional location)
  (let ((path (rtags-path-for-project)))
    (unless location
      (setq location (rtags-current-location)))
    (with-temp-buffer
      (rtags-call-rc path "-N" "-f" location)
      (setq rtags-last-request-not-indexed nil)
      (cond ((= (point-min) (point-max))
             (message "RTags: No target") nil)
            ((string= (buffer-string) "Not indexed\n")
             (setq rtags-last-request-not-indexed t) nil)
            ((string= (buffer-string) "Symbol has moved\n")
             (message "RTags: Symbol has moved") nil)
            (t (buffer-substring (point-min) (- (point-max) 1))))
      )
    )
  )

(defun rtags-setup-filters (filter)
  (setq rtags-path-filter (cond ((stringp filter) filter)
                                (filter buffer-file-name)
                                (t nil)))
  (setq rtags-range-filter (if (and filter
                                    (or (not (= (point-min) 1))
                                        (not (= (point-max) (+ (buffer-size) 1)))))
                               (format "--range-filter=%d-%d" (- (point-min) 1) (- (point-max) 1))
                             nil)))

(defalias 'rtags-find-symbol-at-point 'rtags-follow-symbol-at-point)
(defun rtags-find-symbol-at-point (&optional prefix)
  "Find the natural target for the symbol under the cursor and moves to that location.
For references this means to jump to the definition/declaration of the referenced symbol (it jumps to the definition if it is indexed).
For definitions it jumps to the declaration (if there is only one) For declarations it jumps to the definition.
If called with a prefix restrict to current buffer"
  (interactive "P")
  (rtags-setup-filters prefix)
  (rtags-save-location)
  (let ((target (rtags-target)))
    (rtags-setup-filters nil)
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
  (rtags-setup-filters prefix)
  (rtags-save-location)
  (let ((arg (rtags-current-location)))
    (with-current-buffer (rtags-get-buffer)
      (rtags-call-rc nil "-l" "-r" arg)
      (rtags-setup-filters nil)
      (rtags-handle-completion-buffer))
    )
  )

(defun rtags-find-virtuals-at-point (&optional prefix)
  (interactive "P")
  "List all reimplentations of function under cursor. This includes both declarations and definitions"
  (rtags-setup-filters prefix)
  (rtags-save-location)
  (let ((arg (rtags-current-location)))
    (with-current-buffer (rtags-get-buffer)
      (rtags-call-rc nil "-k" "-l" "-r" arg)
      (rtags-setup-filters nil)
      (rtags-handle-completion-buffer))
    )
  )

(defun rtags-find-all-references-at-point (&optional prefix)
  (interactive "P")
  (rtags-setup-filters prefix)
  (rtags-save-location)
  (let ((arg (rtags-current-location)))
    (with-current-buffer (rtags-get-buffer)
      (rtags-call-rc nil "-l" "-e" "-r" arg)
      (rtags-setup-filters nil)
      (rtags-handle-completion-buffer))
    )
  )

(defun rtags-guess-function-at-point()
  (interactive)
  (rtags-save-location)
  (let ((token (rtags-current-token)))
    (if token
        (with-current-buffer (rtags-get-buffer)
          (rtags-call-rc nil "--declaration-only" "-l" "-F" token)
          (rtags-reset-bookmarks)
          (rtags-handle-completion-buffer t))))
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
        (buffer-substring start (point))))))

(defun rtags-rename-symbol ()
  (interactive)
  (save-some-buffers) ;; it all kinda falls apart when buffers are unsaved
  (let (len file pos destructor replacewith prev (modifications 0) (filesopened 0) replacements)
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
            (setq prev (buffer-substring pos (point)))
            (setq len (- (point) pos))
            (setq replacewith (read-from-minibuffer (format "Replace '%s' with: " prev)))
            (unless (equal replacewith "")
              (if destructor
                  (setq pos (- pos 1)))
              (setq pos (rtags-offset pos))
              (with-temp-buffer
                (rtags-call-rc nil "-e" "-O" "-N" "-r" (format "%s,%d" file pos))
                ;; (message "Got renames %s" (buffer-string))
                (dolist (line (split-string (buffer-string) "\n" t))
                  (if (string-match "^\\(.*\\),\\([0-9]+\\)$" line)
                      (add-to-list 'replacements (cons (match-string 1 line) (string-to-int (match-string 2 line))) t))))
              ;; (message "Got %d replacements" (length replacements))

              (dolist (value replacements)
                (let ((buf (find-buffer-visiting (car value))))
                  (unless buf
                    (progn
                      (incf filesopened)
                      (setq buf (find-file-noselect (car value)))))
                  (when buf
                    (set-buffer buf)
                    (when (run-hook-with-args-until-failure rtags-edit-hook)
                      (incf modifications)
                      (rtags-goto-offset (cdr value))
                      (if (looking-at "~")
                          (forward-char))

                      ;; (message "About to replace %s with %s at %d in %s"
                      ;;          (buffer-substring (point) (+ (point) len)) replacewith (point) (car value))
                      (delete-char len)
                      (insert replacewith)
                      ))
                  )
                )
              )
            )
        )
      )
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

(defun rtags-find-symbol-start () ;; returns column
  (save-excursion
    (let ((looking-at-space (looking-at "[ \t\n]")))
      (skip-chars-backward " \t" (point-at-bol))
      (if (and (> (point) (point-at-bol)) looking-at-space)
          (backward-char))
      (if (looking-at "[A-Za-z0-9_]")
          (c-beginning-of-current-token)
        (forward-char))
    (- (point) (point-at-bol))))
  )

(defun rtags-post-expand ()
  (save-excursion
    (let ((end (point)))
      (backward-char)
      (c-beginning-of-current-token)
      (let ((sig (gethash (buffer-substring (point) end) rtags-completion-signatures)))
        (if sig
            (message "%s" (combine-and-quote-strings sig "\n")))))))

(defun rtags-expand-internal ()
  (save-excursion
    (with-current-buffer rtags-completion
      (if (= (point-min) (point-max))
          (setq rtags-completion nil)
        (progn
          (goto-char (point-min))
          (if (looking-at "Scheduled rebuild")
              (progn
                (setq rtags-completion nil
                      rtags-completion-cache-line 0
                      rtags-completion-cache-column 0
                      rtags-completion-cache-line-contents ""
                      rtags-completion-cache-file-name "")))))))
  (if rtags-completion
      (let ((was-search dabbrev-search-these-buffers-only))
        (condition-case nil
            (progn
              (setq dabbrev-search-these-buffers-only (list rtags-completion))
              (funcall rtags-expand-function)
              (setq dabbrev-search-these-buffers-only was-search)
              (rtags-post-expand))
          (error
           (setq dabbrev-search-these-buffers-only was-search))))
    (when (not (string= rtags-completion-cache-file-name ""))
      (funcall rtags-expand-function)
      (rtags-post-expand)))
  )

(defun rtags-completion-cache-is-valid ()
  (and (= (line-number-at-pos) rtags-completion-cache-line)
       (= (rtags-find-symbol-start) rtags-completion-cache-column)
       (string= (buffer-file-name (current-buffer)) rtags-completion-cache-file-name)
       (string= (buffer-substring (point-at-bol) (+ (point-at-bol) rtags-completion-cache-column))
                rtags-completion-cache-line-contents)))

(defun rtags-expand ()
  (interactive)
  (if rtags-completion
      (rtags-expand-internal)
    (progn
      (funcall rtags-expand-function)
      (rtags-post-expand)))
  )


(defun rtags-prepare-completion ()
  (interactive)
  ;; (message "prepare completion")
  (when rtags-completion-cache-timer
    (cancel-timer rtags-completion-cache-timer)
    (setq rtags-completion-cache-timer nil))
  (when (and (not (rtags-completion-cache-is-valid))
             (rtags-init-completion-stream))
    (if (buffer-live-p rtags-completion)
        (with-current-buffer rtags-completion
          (let (deactivate-mark)
            (erase-buffer))))
    (save-excursion
      ;;(message "prepared completion")
      (let* ((buffer (current-buffer))

             (path (rtags-path-for-project))
             (buffer-size (- (point-max) (point-min)))
             (line (line-number-at-pos))
             (column (rtags-find-symbol-start))
             (header (format "%s:%d:%d:%d:%d\n" (buffer-file-name buffer) line (+ column 1) (- (point) 1) (- (point-max) (point-min)))))
        (setq rtags-completion (get-buffer-create "*RTags Completions*")
              rtags-completion-cache-file-name (buffer-file-name buffer)
              rtags-completion-cache-line line
              rtags-completion-cache-column column
              rtags-completion-cache-line-contents (buffer-substring (point-at-bol) (+ (point-at-bol) column)))
        ;; (message "writing shit %s" header)
        (process-send-string rtags-completion-stream-process header)
        (process-send-string rtags-completion-stream-process (buffer-substring (point-min) (point-max))))
      )
    )
  )

(defvar rtags-diagnostics-process nil)
(defun rtags-apply-fixit-at-point ()
  (interactive)
  (let ((line (buffer-substring (point-at-bol) (point-at-eol))))
    (if (string-match "^\\(.*\\):[0-9]+:[0-9]+: fixit: \\([0-9]+\\)-\\([0-9]+\\): .*did you mean '\\(.*\\)'\\?$" line)
        (let* ((file (match-string 1 line))
               (buf (find-buffer-visiting file))
               (start (string-to-int (match-string 2 line)))
               (end (string-to-int (match-string 3 line)))
               (text (match-string 4 line)))
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
    (when (listp errorlist)
      (-each errorlist #'delete-overlay))
    (puthash filename nil rtags-overlays)))

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
    (libxml-parse-xml-region (point-min) (point-max))))

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
                  (rtags-goto-offset startoffset)
                  (let ((rsym (rtags-current-symbol t)))
                    (when rsym
                      (setq endoffset (+ startoffset (length rsym))))))))

            (let ((overlay (make-overlay (+ startoffset 1) (+ endoffset 1) filebuffer)))
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
              (puthash filename errorlist rtags-overlays)))))))
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
        (--each body (rtags-parse-overlay-error-node it filename))))
    )
  )

(defun rtags-overlays-parse (output)
  (let ((doc (rtags-parse-xml-string output)))
    (when doc
      (unless (eq (car doc) 'checkstyle)
        (error "Unexpected root element %s" (car doc)))
      (-each (cddr doc) #'rtags-parse-overlay-node)))
  )

(defun rtags-check-overlay (overlay)
  (if (and (not (active-minibuffer-window)) (not cursor-in-echo-area))
      (let ((msg (overlay-get overlay 'rtags-error-message)))
        (if msg
          (progn
            (message msg)
            t)
          nil))
    nil)
  )

(defvar rtags-update-current-error-timer nil)

(defun rtags-display-current-error ()
  (let ((current-overlays (overlays-at (point)))
        (done nil))
    (setq rtags-update-current-error-timer nil)
    (--each-while current-overlays (not done) (setq done (rtags-check-overlay it))))
  )

(defun rtags-update-current-error ()
  (if rtags-update-current-error-timer
      (cancel-timer rtags-update-current-error-timer))
  (setq rtags-update-current-error-timer (run-with-idle-timer rtags-error-timer-interval nil (function rtags-display-current-error)))
  )

(add-hook 'post-command-hook (function rtags-update-current-error))

(defun rtags-fix-fixit-overlay (overlay)
  (let ((msg (overlay-get overlay 'rtags-error-message))
        (severity (overlay-get overlay 'rtags-error-severity))
        (start (overlay-get overlay 'rtags-error-start))
        (end (overlay-get overlay 'rtags-error-end)))
    (if (and start end msg (stringp severity) (string= severity "fixit"))
        (if (string-match "did you mean '\\(.*\\)'\\?$" msg)
            (progn
              (save-excursion
                (rtags-goto-offset start)
                (delete-char (- end start))
                (insert (match-string 1 msg)))
              t)
          nil)
      nil))
  )


(defun rtags-fix-fixit-at-point ()
  (interactive)
  (let ((current-overlays (overlays-at (point)))
        (done nil))
    (--each-while current-overlays (not done) (setq done (rtags-fix-fixit-overlay it))))
  )

(defvar rtags-completion-signatures (make-hash-table :test 'equal))
(defvar rtags-completion-buffer-pending nil)

(defun rtags-completion-stream-process-filter (process output)
  (let* ((buf (process-buffer process))
         (process-output t)
         (delimiter (string-match "`" output)))
    (cond (delimiter
           (let (deactivate-mark)
             (with-current-buffer buf
               (erase-buffer)
               (setq rtags-completion-signatures (make-hash-table :test 'equal)
                     rtags-completion-buffer-pending nil
                     output (substring output (+ delimiter 1))))))
          ((string= (substring output 0 (min 21 (length output))) "Scheduled rebuild of ")
           (progn
             (setq rtags-completion nil
                   rtags-completion-cache-line 0
                   rtags-completion-cache-column 0
                   rtags-completion-cache-line-contents ""
                   rtags-completion-cache-file-name ""
                   process-output nil)
             (rtags-restart-completion-cache-timer)))
          (rtags-completion-buffer-pending
           (setq output (concat rtags-completion-buffer-pending output)
                 rtags-completion-buffer-pending nil))
          (t nil))
    (if process-output
        (with-current-buffer buf
          (let ((deactivate-mark) (continue t) (idx 0) (last 0))
            (save-excursion
              (goto-char (point-max))
              (while continue
                (setq idx (string-match "\n" output last))
                (if idx
                    (let* ((ws (string-match " " output last))
                           (key (substring output last ws))
                           (values (gethash key rtags-completion-signatures)))
                      (when (and ws (< (+ ws 1) idx))
                        (setq values (add-to-list 'values (substring output (+ ws 1) idx)))
                        (puthash key values rtags-completion-signatures))
                      (insert (concat key "\n"))
                      (setq last (+ idx 1)))
                  (progn
                    (unless (= last (length output))
                      (setq rtags-completion-buffer-pending (substring output last)))
                    (setq continue nil))
                  )
                )
              )
            )
          )
      )
    )
  )

(defvar rtags-completion-stream-process nil)
(defun rtags-init-completion-stream ()
  (interactive)
  (if (cond ((not rtags-completion-stream-process) t)
            ((eq (process-status rtags-completion-stream-process) 'exit) t)
            ((eq (process-status rtags-completion-stream-process) 'signal) t)
            (t nil))
      (let ((process-connection-type nil))  ; use a pipe
        (if (get-buffer "*RTags Completions*")
            (kill-buffer "*RTags Completions*"))
        (setq rtags-completion-stream-process (start-process
                                               "RTags Completions Stream"
                                               "*RTags Completions*"
                                               (rtags-executable-find "rc")
                                               "--code-complete"))
        (buffer-disable-undo "*RTags Completions*")
        (set-process-filter rtags-completion-stream-process (function rtags-completion-stream-process-filter))
        t)
    t)
  )

(defvar rtags-completion-cache-timer nil)
(defun rtags-restart-completion-cache-timer ()
  (interactive)
  (when (or (eq major-mode 'c++-mode)
            (eq major-mode 'c-mode))
    (if rtags-completion-cache-timer
        (cancel-timer rtags-completion-cache-timer))
    (setq rtags-completion-cache-timer (run-with-idle-timer rtags-completion-timer-interval nil (function rtags-prepare-completion)))
    )
  )

(if rtags-completion-enabled
    (add-hook 'post-command-hook (function rtags-restart-completion-cache-timer))
  (remove-hook 'post-command-hook (function rtags-restart-completion-cache-timer)))

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
            (apply #'start-process "global-update" nil rc arguments))))
      (error (message "Got error in rtags-update-current-project")))
  )

(add-hook 'post-command-hook (function rtags-update-current-project))
;;(remove-hook 'post-command-hook (function rtags-update-current-project))

(defvar rtags-cursorinfo-timer nil)
(defun rtags-restart-cursorinfo-timer ()
  (interactive)
  (when (or (eq major-mode 'c++-mode)
            (eq major-mode 'c-mode))
    (if rtags-cursorinfo-timer
        (cancel-timer rtags-cursorinfo-timer))
    (setq rtags-cursorinfo-cache-timer (run-with-idle-timer rtags-cursorinfo-timer-interval nil (function rtags-update-cursorinfo))))
  )

(if rtags-cursorinfo-timer-enabled
    (add-hook 'post-command-hook (function rtags-restart-cursorinfo-timer))
  (remove-hook 'post-command-hook (function rtags-restart-cursorinfo-timer)))

(defvar rtags-cursorinfo-last-location "")
(defvar rtags-cursorinfo-symbol-name nil)
(defvar rtags-cursorinfo-container-name nil)
(defvar rtags-cursorinfo-container-begin nil)
(defvar rtags-cursorinfo-container-end nil)

(defun rtags-update-cursorinfo ()
  (let ((cur (rtags-current-location)))
    (when (not (equal cur rtags-cursorinfo-last-location))
      (setq rtags-cursorinfo-last-location cur)
      (setq rtags-cursorinfo-symbol-name nil)
      (setq rtags-cursorinfo-container-name nil)
      (let ((cursorinfo (rtags-cursorinfo cur t)))
        (setq rtags-cursorinfo-symbol-name (rtags-current-symbol-name cursorinfo))
        (setq rtags-cursorinfo-container-name (rtags-current-container-name cursorinfo))
        (force-mode-line-update))
      )
    )
  )

(defun rtags-fixup-flymake-err-info ()
  (setq flymake-err-info
        (mapcar (lambda (elem)
                  (let* ((err-info (car (car (cdr elem))))
                         (err-text (flymake-ler-text err-info))
                         (err-type (flymake-ler-type err-info)))
                    ;;(message "err-info is %S" err-info)
                    (when (string-match "^[0-9]+:\\(\s\\|{\\).*\\(warning:\\|error:\\)\\(.*\\)$" err-text)
                      (if (string= (match-string 2 err-text) "warning:")
                          (setq err-type "w"))
                      (setq err-info (flymake-ler-make-ler
                                      (flymake-ler-file err-info)
                                      (flymake-ler-line err-info)
                                      err-type
                                      (concat (match-string 2 err-text) (match-string 3 err-text))
                                      (flymake-ler-full-file err-info)))
                      )
                    ;;(message "err-info became %S" err-info)
                    (list (car elem) (list err-info))))
                flymake-err-info))
)

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
  )

(defvar rtags-pending-diagnostics nil)
(defun rtags-diagnostics-process-filter (process output)
  (let ((errors)
        (oldbuffer (current-buffer))
        (files (make-hash-table)))
    (when rtags-pending-diagnostics
      (setq output (concat rtags-pending-diagnostics output))
      (setq rtags-pending-diagnostics nil))
    (with-current-buffer (process-buffer process)
      (setq buffer-read-only nil)
      ;;(message (format "matching %s" output))
      (let ((endpos (string-match "</checkstyle>" output))
            (proc (get-process "RTags Diagnostics"))
            (current))
        (while (and proc endpos)
          (setq current (substring output 0 (+ endpos 13)))
          (setq output (s-trim-right (substring output (+ endpos 13))))
          (setq endpos (string-match "</checkstyle>" output))
          (rtags-overlays-parse current)))
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
          (setq rtags-diagnostics-process
                (if rtags-autostart-rdm
                    (start-process "RTags Diagnostics" buf (rtags-executable-find "rc") "-m"
                                   (if rtags-rdm-log-enabled "--autostart-rdm=-L/tmp/rdm.log" "--autostart-rdm"))
                  (start-process "RTags Diagnostics" buf (rtags-executable-find "rc") "-m")))
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

(defun rtags-is-indexed (&optional buffer)
  (let ((path (rtags-path-for-project buffer)))
    (with-temp-buffer
      (rtags-call-rc path "-T" path)
      (goto-char (point-min))
      (looking-at "1")))
  )

(defun rtags-has-filemanager (&optional buffer)
  (let ((path (rtags-path-for-project buffer)))
    (with-temp-buffer
      (rtags-call-rc path "--has-filemanager" path)
      (goto-char (point-min))
      (looking-at "1")))
  )


(defun rtags-handle-completion-buffer (&optional noautojump)
  (setq rtags-last-request-not-indexed nil)
  (rtags-reset-bookmarks)
  (cond ((= (point-min) (point-max))
         (message "RTags: No results") nil)
        ((string= (buffer-string) "Not indexed\n")
         (setq rtags-last-request-not-indexed t) nil)
        ((string= (buffer-string) "Symbol has moved\n")
         (message "RTags: Symbol has moved") nil)
        ((= (count-lines (point-min) (point-max)) 1)
         (let ((string (buffer-string)))
           (bury-buffer)
           (rtags-goto-location string)))
        (t (progn
             (switch-to-buffer-other-window rtags-buffer-name)
             (shrink-window-if-larger-than-buffer)
             (goto-char (point-min))
             (rtags-start-mode t)
             (setq rtags-no-otherbuffer nil)
             (if (and rtags-jump-to-first-match (not noautojump))
                 (rtags-select-other-buffer))))
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
        (setq string (match-string 1 string)))
    (with-temp-buffer
      (rtags-call-rc default-directory "-P" string (if rtags-find-file-case-insensitive "-I"))

      (goto-char (point-min))
      (if (equal "" string)
          (while (not (eobp))
            (intern (buffer-substring (point-at-bol) (point-at-eol)) complete-list)
            (forward-line))
        (let ((match-string (format  ".*\\(%s.*\\)" string)))
          (while (not (eobp))
            (if (looking-at match-string)
                (intern (buffer-substring (match-beginning 1) (match-end 1)) complete-list))
            (forward-line))))
      (cond ((eq code nil)
             (try-completion string complete-list predicate))
            ((eq code t)
             (all-completions string complete-list predicate))
            ((eq code 'lambda)
             (if (intern-soft string complete-list) t nil))))))


(defun rtags-select (&optional otherbuffer)
  (interactive "P")
  (let* ((line (line-number-at-pos))
         (bookmark (format "R_%d" line)))
    (if (and (>= rtags-buffer-bookmarks line)
             (member bookmark (bookmark-all-names)))
        (progn
          (when otherbuffer
            (if (= (length (window-list)) 1)
                (split-window))
            (other-window 1))
          (bookmark-jump bookmark)
          (rtags-location-stack-push))
      (rtags-goto-location (buffer-substring (point-at-bol) (point-at-eol)) nil otherbuffer))
    )
  )

(defun rtags-select-other-buffer (&optional nototherbuffer)
  (interactive "P")
  (rtags-select (not nototherbuffer))
  )

(defun rtags-select-and-remove-rtags-buffer ()
  (interactive)
  (let ((line (buffer-substring (point-at-bol) (point-at-eol))))
    (delete-window) ;; ### this should really use rtags-select so it would get bookmarks
    (rtags-goto-location line)))

(defun rtags-imenu ()
  (interactive)
  (rtags-save-location)
  (rtags-setup-filters (buffer-file-name))
  (let* ((alternatives (with-temp-buffer
                        (rtags-call-rc default-directory "--imenu" "--list-symbols" "-Y")
                        (eval (read (buffer-string)))))
         (match (car alternatives)))
    (if (> (length alternatives) 1)
        (setq match (ido-completing-read "Symbol: " alternatives)))
    (if match
        (rtags-goto-location (with-temp-buffer (rtags-call-rc default-directory "-F" match) (buffer-string)))
      (message "RTags: No symbols")))
  (rtags-setup-filters nil))

(defvar rtags-find-file-history nil)
(defun rtags-find-file (&optional prefix tagname)
  (interactive "P")
  (rtags-save-location)
  (rtags-setup-filters nil)
  (let ((tagname (rtags-current-symbol t)) prompt input offset line column
        (prefer-exact rtags-find-file-prefer-exact-match))
    (if prefix
        (setq prefer-exact (not prefer-exact)))
    (if tagname
        (setq prompt (concat (format "Find rfiles (default %s): " tagname)))
      (setq prompt "Find rfiles: "))
    (rtags-is-indexed)
    (setq input (completing-read prompt (function rtags-filename-complete) nil nil nil 'rtags-find-file-history))
    (setq rtags-find-file-history (remove-duplicates rtags-find-file-history :from-end t :test 'equal))
    (cond ((string-match "\\(.*\\),\\([0-9]+\\)" input)
           (progn
             (setq tagname (match-string 1 input))
             (setq offset (string-to-int (match-string 2 input)))))
          ((string-match "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)" input)
           (progn
             (setq tagname (match-string 1 input))
             (setq line (string-to-int (match-string 2 input)))
             (setq column (string-to-int (match-string 3 input)))))
          ((string-match "\\(.*\\):\\([0-9]+\\)" input)
           (setq tagname (match-string 1 input))
           (setq line (string-to-int (match-string 2 input))))
          ((not (equal "" input))
           (setq tagname input))
          (t nil))

    ;; (message (format "%s %s %d" input tagname rtags-find-file-offset))
    (rtags-reset-bookmarks)
    (rtags-location-stack-push)

    (with-current-buffer (rtags-get-buffer)
      (rtags-call-rc default-directory "-K" "-P" tagname
                     (if rtags-find-file-case-insensitive "-I")
                     (if prefer-exact "-A"))
      (cond (offset (replace-regexp "$" (format ",%d" offset)))
            ((and line column) (replace-regexp "$" (format ":%d:%d" line column)))
            ((and line) (replace-regexp "$" (format ":%d" line)))
            (t nil))
      ;; (message (format "Got lines and shit %d\n[%s]" (count-lines (point-min) (point-max)) (buffer-string)))
      (cond ((= (point-min) (point-max)) t)
            ((= (count-lines (point-min) (point-max)) 1) (rtags-goto-location (buffer-substring (point-at-bol) (point-at-eol))))
            (t (progn
                 (switch-to-buffer-other-window rtags-buffer-name)
                 (shrink-window-if-larger-than-buffer)
                 (rtags-start-mode nil)
                 ;; (setq rtags-no-otherbuffer t)
                 )))
      ;; Should add support for putting offset in there as well, ignore it on completion and apply it at the end
      )
    )
  )

(defun rtags-show-rtags-buffer ()
  (interactive)
  (if (get-buffer "*RTags*")
      (display-buffer "*RTags*")))

(defun rtags-fixit (&optional ediff buffer)
  (interactive "P")
  (save-some-buffers)
  (unless buffer
    (setq buffer (current-buffer)))
  (save-excursion
    (let* ((path (buffer-file-name buffer))
           (tempbuf nil)
           (buffertext (if ediff (with-current-buffer buffer (buffer-string))))
           (min (- (if mark-active (region-beginning) (point-min)) 1))
           (max (- (if mark-active (region-end) (point-max)) 1))
           (line nil))
      (with-temp-buffer
        (rtags-call-rc path "--fixit" path)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring (point-at-bol) (point-at-eol))))
            (if (string-match "^\\([0-9]+\\)-\\([0-9]+\\) \\(.*\\)$" line)
                (let ((start (string-to-int (match-string 1 line)))
                      (end (string-to-int (match-string 2 line)))
                      (text (match-string 3 line)))
                  (when (and (>= start min) (< start max))
                    (when (not (or (not ediff) tempbuf))
                      (setq tempbuf (rtags-get-buffer (format "*RTags Fixit - %s *" path)))
                      (with-current-buffer tempbuf
                        (insert buffertext)))
                    (save-excursion
                      (set-buffer (or tempbuf buffer))
                      (rtags-goto-offset start)
                      (delete-char (- end start)) ;; may be 0
                      (insert text))))))
          ;; (message (format "got something %d to %d => [%s]" start end text))))
          (next-line))
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
        (match-string 1 cursorinfo))
    )
  )

(defun rtags-current-container-name (&optional cursorinfo)
  (unless cursorinfo
    (setq cursorinfo (rtags-cursorinfo)))
  (let* ((container (string-match "^Container:" cursorinfo))
         (symbolname (string-match "^SymbolName: \\(.*\\)$" cursorinfo (if container container 0))))
    (if container
        (match-string 1 cursorinfo)
      nil)))

(defun rtags-cursor-extent (&optional location)
  (let ((cursorinfo (rtags-cursorinfo location)))
    (if (string-match "^Range: \\([0-9]+\\)-\\([0-9]+\\)$" cursorinfo)
        (let ((start (+ (string-to-int (match-string 2 cursorinfo)) 1))
              (end (+ (string-to-int (match-string 3 cursorinfo)) 1)))
          (cons start end)))))

(defun rtags-target-content (&optional location)
  (let ((cursorinfo (rtags-cursorinfo (rtags-target location))) file)
    (if (string-match "^\\(/[^ ]*\\),[0-9]+ " cursorinfo)
        (progn
          (setq file match-string 1 cursorinfo)
          (if (string-match "^Range: \\([0-9]+\\)-\\([0-9]+\\)$" cursorinfo)
              (let ((start (+ (string-to-int (match-string 2 cursorinfo)) 1))
                    (end (+ (string-to-int (match-string 3 cursorinfo)) 1)))
                (with-temp-buffer
                  (insert-file-contents file start end)
                  (buffer-string))))))))

;; (defun rtags-tooltip ()
;;   (interactive)
;;   (let ((cursorinfo (rtags-cursorinfo (rtags-target))))
;;     (message cursorinfo)
;;     (if (rtags-target)
;;         (message (rtags-target)))
;;     (if (string-match "^\\(.*\\),[0-9]+ CursorInfo(\\([0-9]+\\)-\\([0-9]+\\) " cursorinfo)
;;         (let ((file (match-string 1 cursorinfo))
;;               (start (+ (string-to-int (match-string 2 cursorinfo)) 1))
;;               (end (+ (string-to-int (match-string 3 cursorinfo)) 1))
;;               (tip))
;;           (save-excursion
;;             (find-file file)
;;             (setq tip (buffer-substring start end))
;;             tip)
;;           )
;;       )
;;     )
;;   )

(defvar rtags-other-buffer-window nil)
(defun rtags-remove-other-buffer ()
  (interactive)
  (let ((ret ""))
    (if (and (> (length (window-list nil nil)) 1)
             rtags-other-buffer-window
             (window-live-p rtags-other-buffer-window))
        (progn
          (select-window rtags-other-buffer-window)
          (setq ret (rtags-current-location))
          (delete-window rtags-other-buffer-window)
          (setq rtags-other-buffer-window nil)))
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

(defcustom rtags-other-buffer-window-size-percentage 30 "Percentage size of other buffer" :group 'rtags :type 'integer)
(defun rtags-show-target-in-other-buffer ()
  (interactive)
  (let ((target (rtags-target)))
    (unless target
      (let ((token (rtags-current-token)))
        (if token
            (with-temp-buffer
              (rtags-call-rc nil "--declaration-only" "-N" "-F" token)
              (if (= (count-lines (point-min) (point-max)) 1)
                  (setq target (buffer-substring (point) (- (point-max) 1))))))))
    (if target
        (let ((other-buffer-content (rtags-remove-other-buffer))
              (win (selected-window))
              (height (* (window-height) (- 100 rtags-other-buffer-window-size-percentage))))
          (unless (string= target other-buffer-content)
            (progn
              (setq height (/ height 100))
              (setq rtags-other-buffer-window (split-window nil height))
              (select-window rtags-other-buffer-window)
              (rtags-goto-location target)
              (recenter-top-bottom 0)
              (select-window win)))))))

(provide 'rtags)
