(defgroup rtags nil
  "Minor mode for rtags."
  :group 'tools
  :prefix "rtags-")

(require 'cl)
(require 'ido)
(require 'dabbrev)
(require 'cc-mode)
(require 'flymake)

(defvar rtags-last-buffer nil)
(defvar rtags-path-filter nil)
(defvar rtags-path-filter-regex nil)
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

(defvar rtags-faces
  '(("^[^ ]*" . rtags-path-face)
    (" .*$" . rtags-context-face))
  )

(defun rtags-bury-or-delete ()
  (interactive)
  (if (> (length (window-list nil nil)) 1)
      (delete-window)
    (bury-buffer)))

(defvar rtags-mode-map nil)
;; assign command to keys
(setq rtags-mode-map (make-sparse-keymap))
(define-key rtags-mode-map (kbd "RET") 'rtags-select-other-buffer)
(define-key rtags-mode-map (kbd "ENTER") 'rtags-select-other-buffer)
(define-key rtags-mode-map (kbd "SPC") 'rtags-select)
(define-key rtags-mode-map (kbd "q") 'rtags-bury-or-delete)
(define-key rtags-mode-map (kbd "j") 'next-line)
(define-key rtags-mode-map (kbd "k") 'previous-line)

(define-derived-mode rtags-mode fundamental-mode
  (setq font-lock-defaults '(rtags-faces))
  (setq mode-name "rtags")
  (use-local-map rtags-mode-map)
  (run-hooks 'rtags-mode-hook)
  (goto-char (point-max))
  (if (= (point-at-bol) (point-max))
      (delete-char -1))
  (goto-char (point-min))
  (setq buffer-read-only t)
  )

(defun rtags-next-match () (interactive) (rtags-next-prev-match t))
(defun rtags-previous-match () (interactive) (rtags-next-prev-match nil))

(defun rtags-next-prev-match (next)
  (if (get-buffer rtags-buffer-name)
      (let (target
            (win (get-buffer-window rtags-buffer-name)))
        (if win (select-window win))
        (set-buffer rtags-buffer-name)
        (when (> (point-max) (point-min))
          (cond ((and (= (point-at-bol) (point-min)) (not next))
                 (setq target (point-max))
                 (message "*RTags* Wrapped"))
                ((and (= (point-at-eol) (point-max)) next)
                 (setq target (point-min))
                 (message "*RTags* Wrapped"))
                (next
                 (setq target (point-at-bol 2)))
                (t
                 (setq target (point-at-bol -1))))
          (goto-char target)
          (beginning-of-line)
          (if win (rtags-select-other-buffer) (rtags-select))))))

(defun rtags-executable-find (exe)
  (let ((result (if rtags-path (concat rtags-path "/" exe) (executable-find exe))))
    (if (file-exists-p result) result nil)))

(defun rtags-call-rc (path &rest arguments)
  (apply #'rtags-call-rc-helper path nil t arguments))

(defun rtags-call-rc-unsaved (path unsaved-pos output &rest arguments)
  (apply #'rtags-call-rc-helper path unsaved-pos output arguments))

(defun rtags-call-rc-helper (path unsaved-pos output &rest arguments)
  (save-excursion
    (let ((rc (rtags-executable-find "rc")))
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
            (> (point-max) (point-min)))))))

(defun rtags-path-for-project (&optional buffer)
  (expand-file-name (if (buffer-file-name buffer)
                        (buffer-file-name buffer)
                      default-directory)))

(defun rtags-preprocess-file (&optional buffer)
  (interactive)
  (let ((fn (buffer-file-name buffer))
        bufname narrow-start narrow-end)
    (if (and mark-active
             (not (equal (region-beginning) (region-end))))
        (setq narrow-start (+ 1 (count-lines (point-min) (region-beginning)))
              narrow-end (+ 1 (count-lines (point-min) (region-end)))))
    (if fn
        (let ((preprocess-buffer (get-buffer-create (format "*RTags preprocessed %s*" fn))))
          (with-current-buffer preprocess-buffer
            (setq buffer-read-only nil)
            (erase-buffer)
            (rtags-call-rc nil "--preprocess" fn)
            (if (and narrow-start narrow-end)
                (let ((match-regexp (concat "^# \\([0-9]*\\) \"" (file-truename fn) "\""))
                      last-match last-line start end)
                  (while (re-search-forward match-regexp nil t)
                    (let ((current-line (string-to-int (match-string 1))))
                      (if (and (not start) (> current-line narrow-start)) (setq start (+ (count-lines (point-min) last-match) (- narrow-start last-line))))
                      (if (and (not end) (> current-line narrow-end)) (setq end (+ (count-lines (point-min) last-match) (- narrow-end last-line))))
                      (setq last-line current-line)
                      (setq last-match (point))))
                  (if last-match
                      (progn
                        (if (not start) (setq start (+ (count-lines (point-min) last-match) (- narrow-start last-line))))
                        (if (not end) (setq end (+ (count-lines (point-min) last-match) (- narrow-end last-line))))))
                  (if (and start end)
                      (progn
                        (goto-char (point-min))
                        (narrow-to-region (point-at-bol (+ start 1)) (point-at-bol (+ end 1)))))))
            (setq buffer-read-only t)
            (local-set-key "q" 'bury-buffer)
            (c++-mode)
            )
          (display-buffer preprocess-buffer))
      )))

(defun rtags-reparse-file(&optional buffer)
  (interactive)
  (let ((path (rtags-path-for-project)))
    (with-temp-buffer
      (rtags-call-rc path "-V" (buffer-name buffer)))
    (message (format "Dirtied %s" (buffer-name buffer)))
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

(defun rtags-find-ancestor-file(pattern)
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

(defun rtags-find-ancestor-file-directory(pattern)
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

(defun rtags-cursorinfo (&optional location)
  (let ((loc (if location location (rtags-current-location)))
        (path (rtags-path-for-project)))
    (with-temp-buffer
      (rtags-call-rc path "-U" loc)
      (buffer-string))))

(defun rtags-print-cursorinfo (&optional location)
  (interactive)
  (message "%s" (rtags-cursorinfo)))


(defun rtags-current-location ()
  (format "%s,%d" (buffer-file-name) (- (point) 1)))

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

(defun rtags-save-location()
  (setq rtags-last-buffer (current-buffer))
  (rtags-bookmark-push))

(defun rtags-goto-location(location &optional nobookmark &optional otherbuffer)
  ;;  (message (format "rtags-goto-location \"%s\"" location))
  (if (length location)
      (progn
        (if rtags-no-otherbuffer (setq otherbuffer nil))
        "Go to a location passed in. It can be either: file,12 or file:13:14 or plain file"
        (cond ((string-match "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)" location)
               (let ((line (string-to-int (match-string 2 location)))
                     (column (string-to-int (match-string 3 location))))
                 (if otherbuffer
                     (find-file-other-window (match-string 1 location))
                   (find-file (match-string 1 location)))
                 (run-hooks rtags-after-find-file-hook)
                 (goto-char (point-min))
                 (forward-line (- line 1))
                 (forward-char (- column 1))
                 (unless nobookmark (rtags-bookmark-push))
                 t))
              ((string-match "\\(.*\\):\\([0-9]+\\)" location)
               (let ((line (string-to-int (match-string 2 location))))
                 (if otherbuffer
                     (find-file-other-window (match-string 1 location))
                   (find-file (match-string 1 location)))
                 (run-hooks rtags-after-find-file-hook)
                 (goto-char (point-min))
                 (forward-line (- line 1))
                 (unless nobookmark (rtags-bookmark-push))
                 t))
              ((string-match "\\(.*\\),\\([0-9]+\\)" location)
               (let ((offset (string-to-int (match-string 2 location))))
                 (if otherbuffer
                     (find-file-other-window (match-string 1 location))
                   (find-file (match-string 1 location)))
                 (run-hooks rtags-after-find-file-hook)
                 (goto-char (+ offset 1))
                 (unless nobookmark (rtags-bookmark-push))
                 t))
              (t
               (if otherbuffer
                   (find-file-other-window location)
                 (find-file location))
               )
              )
        )
    )
  )

(defun rtags-find-symbols-by-name-internal (p references pathfilter)
  (rtags-save-location)
  (setq rtags-path-filter pathfilter)
  (let ((tagname (rtags-current-symbol))
        (switch (if references "-R" "-F"))
        (path (rtags-path-for-project))
        prompt
        input)
    (if tagname
        (setq prompt (concat p ": (default " tagname ") "))
      (setq prompt (concat p ": ")))
    (setq input (completing-read prompt (function rtags-symbolname-complete) nil nil nil rtags-symbol-history))
    (if (not (equal "" input))
        (setq tagname input))
    (if (get-buffer rtags-buffer-name)
        (kill-buffer rtags-buffer-name))
    (with-current-buffer (generate-new-buffer rtags-buffer-name)
      (rtags-call-rc path switch tagname "-l")
      (setq rtags-path-filter nil)
      (rtags-handle-completion-buffer)
      )
    )
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

(defvar rtags-bookmark-index 0)
(defvar rtags-bookmarks nil)

(defun rtags-bookmark-push ()
  (let ((bm (rtags-current-location)))
    (while (> rtags-bookmark-index 0)
      (progn
        (setq rtags-bookmark-index (- rtags-bookmark-index 1))
        (pop rtags-bookmarks)
        )
      )
    (unless (string= bm (nth 0 rtags-bookmarks))
      (push bm rtags-bookmarks)
      (if (> (length rtags-bookmarks) rtags-max-bookmark-count)
          (nbutlast rtags-bookmarks (- (length rtags-bookmarks) rtags-max-bookmark-count))
        )
      )
    )
  )

(defun rtags-bookmark-jump (by)
  (interactive)
  (let ((instack (nth rtags-bookmark-index rtags-bookmarks))
        (cur (rtags-current-location)))
    (if (not (string= instack cur))
        (rtags-goto-location instack t)
      (let ((target (+ rtags-bookmark-index by)))
        (if (and (>= target 0) (< target (length rtags-bookmarks)))
            (progn
              (setq rtags-bookmark-index target)
              (rtags-goto-location (nth rtags-bookmark-index rtags-bookmarks) t)
              )
          )
        )
      )
    )
  )

;; **************************** API *********************************

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

(defcustom rtags-expand-function '(lambda() (dabbrev-expand nil))
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

(defcustom rtags-autostart-rdm t
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
  (define-key map (kbd "C-x r /") (function rtags-find-all-references-at-point))
  (define-key map (kbd "C-x r >") (function rtags-find-symbol))
  (define-key map (kbd "C-x r <") (function rtags-find-references))
  (define-key map (kbd "C-x r [") (function rtags-bookmark-back))
  (define-key map (kbd "C-x r ]") (function rtags-bookmark-forward))
  (define-key map (kbd "C-x r C") (function rtags-switch-to-completion-buffer))
  (define-key map (kbd "C-x r D") (function rtags-diagnostics))
  (define-key map (kbd "C-x r G") (function rtags-clear-diagnostics))
  (define-key map (kbd "C-x r M") (function rtags-index-project))
  (define-key map (kbd "C-x r p") (function rtags-set-current-project))
  (define-key map (kbd "C-x r e") (function rtags-reparse-file))
  (define-key map (kbd "C-x r E") (function rtags-preprocess-file))
  (define-key map (kbd "C-x r R") (function rtags-rename-symbol))
  (define-key map (kbd "C-x r U") (function rtags-print-cursorinfo))
  (define-key map (kbd "C-x r O") (function rtags-goto-offset))
  (define-key map (kbd "C-x r ;") (function rtags-find-file))
  (define-key map (kbd "C-x r F") (function rtags-fixit))
  )

(defun rtags-print-current-location ()
  (interactive)
  (message (rtags-current-location)))

(defun rtags-quit-rdm () (interactive)
  (call-process (rtags-executable-find "rc") nil nil nil "--quit-rdm"))

(defun rtags-switch-to-completion-buffer () (interactive)
  (let ((buf (get-buffer "*RTags Completions*")))
    (switch-to-buffer-other-window buf)))

(defun rtags-bookmark-forward()
  (interactive)
  (rtags-bookmark-jump -1)
  )

(defun rtags-bookmark-back()
  (interactive)
  (rtags-bookmark-jump 1)
  )

(defun rtags-bookmarks-reset ()
  (interactive)
  (setq rtags-bookmarks nil)
  (setq rtags-bookmark-index 0)
  )

(defun rtags-index-project ()
  (interactive)
  (let ((makefile (read-file-name
                   "Index project Makefile: "
                   default-directory
                   nil
                   t
                   (if (file-exists-p (concat default-directory "/Makefile")) "Makefile" nil))))
    (if (file-exists-p makefile)
        (with-temp-buffer
          (rtags-call-rc nil "-m" makefile)
          (eessage (buffer-string))))))

(defun rtags-target (&optional location)
  (let ((path (rtags-path-for-project)))
    (unless location
      (setq location (rtags-current-location)))
    (with-temp-buffer
      (rtags-call-rc path "-N" "-f" location)
      (if (< (point-min) (point-max))
          (let ((loc (buffer-substring (point-min) (- (point-max) 1))))
            (if (string= loc "Not indexed")
                (progn
                  (setq rtags-last-request-not-indexed t)
                  nil)
              (setq rtags-last-request-not-indexed nil)
              loc))
        (progn
          (setq rtags-last-request-not-indexed nil)
          nil)))))

(defalias 'rtags-find-symbol-at-point 'rtags-follow-symbol-at-point)
(defun rtags-find-symbol-at-point (&optional prefix)
  (interactive "P")
  (setq rtags-path-filter (if prefix buffer-file-name nil))
  (rtags-save-location)
  (let ((target (rtags-target)))
    (if target
        (rtags-goto-location target))
    )
  )

(defun rtags-find-references-at-point(&optional prefix)
  (interactive "P")
  (setq rtags-path-filter (if prefix buffer-file-name nil))
  (rtags-save-location)
  (let ((arg (rtags-current-location)))
    (if (get-buffer rtags-buffer-name)
        (kill-buffer rtags-buffer-name))
    (with-current-buffer (generate-new-buffer rtags-buffer-name)
      (rtags-call-rc nil "-l" "-r" arg)
      (setq rtags-path-filter nil)
      (if (< (point-min) (point-max))
          (let ((loc (buffer-substring (point-min) (- (point-max) 1))))
            (if (string= loc "Not indexed")
                (progn
                  (setq rtags-last-request-not-indexed t)
                  (erase-buffer))
              (setq rtags-last-request-not-indexed nil))))
      (rtags-handle-completion-buffer))
    )
  )

(defun rtags-find-virtuals-at-point(&optional prefix)
  (interactive "P")
  (setq rtags-path-filter (if prefix buffer-file-name nil))
  (rtags-save-location)
  (let ((arg (rtags-current-location)))
    (if (get-buffer rtags-buffer-name)
        (kill-buffer rtags-buffer-name))
    (with-current-buffer (generate-new-buffer rtags-buffer-name)
      (rtags-call-rc nil "-k" "-l" "-r" arg)
      (setq rtags-path-filter nil)
      (rtags-handle-completion-buffer))
    )
  )

(defun rtags-find-all-references-at-point(&optional prefix)
  (interactive "P")
  (setq rtags-path-filter (if prefix buffer-file-name nil))
  (rtags-save-location)
  (let ((arg (rtags-current-location)))
    (if (get-buffer rtags-buffer-name)
        (kill-buffer rtags-buffer-name))
    (with-current-buffer (generate-new-buffer rtags-buffer-name)
      (rtags-call-rc nil "-l" "-E" "-r" arg)
      (rtags-handle-completion-buffer))
    )
  )

(defun rtags-rename-symbol ()
  (interactive)
  (save-some-buffers) ;; it all kinda falls apart when buffers are unsaved
  (let (len file pos destructor replacewith prev (modifications 0) (filesopened 0))
    (save-excursion
      (if (looking-at "[0-9A-Za-z_~#]")
          (progn
            (while (and (> (point) 1) (looking-at "[0-9A-Za-z_~#]"))
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
              (with-temp-buffer
                (rtags-call-rc nil "-E" "-O" "-N" "-r" (format "%s,%d" file (- pos 1)))
                (while (looking-at "^\\(.*\\),\\([0-9]+\\)$")
                  ;;(message (buffer-substring (point-at-bol) (point-at-eol)))
                  (let ((fn (match-string 1))
                        (p (string-to-number (match-string 2)))
                        (buf nil))
                    (setq buf (find-buffer-visiting fn))
                    (unless buf
                      (progn
                        (incf filesopened)
                        (setq buf (find-file-noselect fn))))
                    (if buf
                        (save-excursion
                          (set-buffer buf)
                          (if (run-hook-with-args-until-failure rtags-edit-hook)
                              (progn
                                (incf modifications)
                                (goto-char (+ p 1))
                                (if (looking-at "~")
                                    (forward-char))

                                ;; (message (format "About to replace %s with %s at %d in %s"
                                ;;                  (buffer-substring (point) (+ (point) len))
                                ;;                  replacewith
                                ;;                  (point)
                                ;;                  fn))
                                (delete-char len)
                                (insert replacewith)
                                ))
                          )))
                  (next-line))
                )))
        (message (format "Opened %d new files and made %d modifications" filesopened modifications))))))

(defun rtags-find-symbol (&optional prefix)
  (interactive "P")
  (rtags-find-symbols-by-name-internal "Find rsymbol" nil (if prefix buffer-file-name nil)))

(defun rtags-find-references (&optional prefix)
  (interactive "P")
  (rtags-find-symbols-by-name-internal "Find rreferences" t (if prefix buffer-file-name nil)))

(defun rtags-find-symbol-current-file ()
  (interactive)
  (rtags-find-symbol t))

(defun rtags-find-references-current-file ()
  (interactive)
  (rtags-find-references t))

(defun rtags-dir-filter()
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

(defun rtags-find-symbol-start() ;; returns column
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

(defun rtags-post-expand()
  (save-excursion
    (let ((end (point)))
      (backward-char)
      (c-beginning-of-current-token)
      (let ((sig (gethash (buffer-substring (point) end) rtags-completion-signatures)))
        (if sig
            (message "%s" (combine-and-quote-strings sig "\n")))))))

(defun rtags-expand-internal()
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

(defun rtags-expand()
  (interactive)
  (if rtags-completion
      (rtags-expand-internal)
    (progn
      (funcall rtags-expand-function)
      (rtags-post-expand)))
  )


(defun rtags-prepare-completion()
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

(defun rtags-stop-diagnostics ()
  (interactive)
  (if (and rtags-diagnostics-process (not (eq (process-status rtags-diagnostics-process) 'exit)))
      (kill-process rtags-diagnostics-process))
  (if (get-buffer "*RTags Diagnostics*")
      (kill-buffer "*RTags Diagnostics*")))

(defun rtags-clear-diagnostics ()
  (interactive)
  (if (get-buffer "*RTags Diagnostics*")
      (with-current-buffer "*RTags Diagnostics*"
        (setq buffer-read-only nil)
        (goto-char (point-min))
        (delete-char (- (point-max) (point-min)))
        (setq buffer-read-only t))
    )
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
    (add-hook 'post-command-hook (function rtags-restart-completion-cache-timer)))

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
    (add-hook 'post-command-hook (function rtags-restart-cursorinfo-timer)))

(defvar rtags-cursorinfo-last-location "")
(defvar rtags-cursorinfo-symbol-name nil)
(defvar rtags-cursorinfo-container-name nil)
(defvar rtags-cursorinfo-container-begin nil)
(defvar rtags-cursorinfo-container-end nil)

(defun rtags-update-cursorinfo ()
  (let ((cur (rtags-current-location)))
    (if (not (equal cur rtags-cursorinfo-last-location))
        (let ((cursorinfo (rtags-cursorinfo cur)))
          (setq rtags-cursorinfo-last-location cur)
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

(defun rtags-diagnostics-process-filter (process output)
  (let ((dollar (string-match "\\$\s*$" output)))
    ;; (message "%s %d" output dollar)
    (if dollar
        (setq output (substring output 0 dollar)))

    (if (> (length output) 0)
        (flymake-parse-output-and-residual output))

    (when dollar
      (flymake-parse-residual)
      (setq flymake-err-info flymake-new-err-info)
      (setq flymake-new-err-info nil)
      (setq flymake-err-info (flymake-fix-line-numbers flymake-err-info 1 (flymake-count-lines)))
      (flymake-delete-own-overlays)
      (rtags-fixup-flymake-err-info)
      (flymake-highlight-err-lines flymake-err-info)))

  (with-current-buffer (process-buffer process)
    (setq buffer-read-only nil)
    (let (deactivate-mark)
      (save-excursion
        (goto-char (point-max))
        (insert output)))
    (setq buffer-read-only t))
  )


(defun rtags-init-diagnostics-buffer-and-process ()
  (let ((buf (get-buffer-create "*RTags Diagnostics*")))
    (with-current-buffer buf
      (setq buffer-read-only t)
      (compilation-mode) ;; ### hmm
      (local-set-key "c" 'rtags-clear-diagnostics)
      (local-set-key "q" 'bury-buffer))
    (if (cond ((not rtags-diagnostics-process) t)
              ((eq (process-status rtags-diagnostics-process) 'exit) t)
              ((eq (process-status rtags-diagnostics-process) 'signal) t)
              (t nil))
        (let ((process-connection-type nil))  ; use a pipe
          (setq rtags-diagnostics-process
                (if rtags-autostart-rdm
                    (start-process "RTags Diagnostics" buf (rtags-executable-find "rc") "-G"
                                   (if rtags-rdm-log-enabled "--autostart-rdm=-L/tmp/rdm.log" "--autostart-rdm"))
                  (start-process "RTags Diagnostics" buf (rtags-executable-find "rc") "-G")))
          (set-process-filter rtags-diagnostics-process (function rtags-diagnostics-process-filter))
          (rtags-clear-diagnostics))
      )
    )
  )

(defun rtags-diagnostics (&optional restart)
  (interactive)
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


(defun rtags-handle-completion-buffer ()
  (let ((empty (= (point-min) (point-max))))
    (cond (empty t)
          ((= (count-lines (point-min) (point-max)) 1)
           (let ((string (buffer-string)))
             (bury-buffer)
             (rtags-goto-location string)))
          (t (progn
               (switch-to-buffer-other-window rtags-buffer-name)
               (shrink-window-if-larger-than-buffer)
               (goto-char (point-min))
               (rtags-mode)
               (setq rtags-no-otherbuffer nil)
               (if rtags-jump-to-first-match
                   (rtags-select-other-buffer)))))
    (not empty))
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
      (rtags-call-rc nil "-P" string (if rtags-find-file-case-insensitive "-c"))

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


(defun rtags-select()
  (interactive)
  (let ((file (buffer-substring (point-at-bol) (point-at-eol))))
    (if (length file)
        (progn
          (bury-buffer)
          (rtags-goto-location file)))))

(defun rtags-select-other-buffer()
  (interactive)
  (rtags-goto-location (buffer-substring (point-at-bol) (point-at-eol)) nil t))

(defvar rtags-find-file-history nil)
(defun rtags-find-file (&optional prefix tagname)
  (interactive "P")
  (rtags-save-location)
  (let ((tagname (rtags-current-symbol t)) prompt input offset line column
        (prefer-exact rtags-find-file-prefer-exact-match))
    (if prefix
        (setq prefer-exact (not prefer-exact)))
    (if tagname
        (setq prompt (concat (format "Find rfiles (default %s): " tagname)))
      (setq prompt "Find rfiles: "))
    (setq input (completing-read prompt (function rtags-filename-complete) nil nil nil rtags-find-file-history))
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
    (if (get-buffer rtags-buffer-name)
        (kill-buffer rtags-buffer-name))
    (with-current-buffer (generate-new-buffer rtags-buffer-name)
      ;; (let ((args (list "-K" "-P" tagname)))
      ;;   (if rtags-find-file-case-insensitive
      ;;       (push "-c" args))
      ;;   (if prefer-exact
      ;;       (push "-c" args))

      (rtags-call-rc nil "-K" "-P" tagname
                     (if rtags-find-file-case-insensitive "-c")
                     (if prefer-exact "-A"))
      (cond (offset (replace-regexp "$" (format ",%d" offset)))
            ((and line column) (replace-regexp "$" (format ":%d:%d" line column)))
            ((and line) (replace-regexp "$" (format ":%d" line)))
            (t nil))
      ;; (message (format "Got lines and shit %d\n[%s]" (count-lines (point-min) (point-max)) (buffer-string)))
      (cond ((= (point-min) (point-max)) t)
            ((= (count-lines (point-min) (point-max)) 1) (rtags-select))
            (t (progn
                 (switch-to-buffer-other-window rtags-buffer-name)
                 (shrink-window-if-larger-than-buffer)
                 (rtags-mode)
                 ;; (setq rtags-no-otherbuffer t)
                 )))
      ;; Should add support for putting offset in there as well, ignore it on completion and apply it at the end
      )
    )
  )

(defun rtags-fixit (&optional buffer)
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))
  (save-excursion
    (let ((path (buffer-file-name buffer)) line)
      (with-temp-buffer
        (rtags-call-rc path "--fixit" path)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring (point-at-bol) (point-at-eol))))
            (if (string-match "^\\([0-9]+\\)-\\([0-9]+\\) \\(.*\\)$" line)
                (let ((start (string-to-int (match-string 1 line)))
                      (end (string-to-int (match-string 2 line)))
                      (text (match-string 3 line)))
                  (save-excursion
                    (set-buffer buffer)
                    (goto-char (+ start 1)) ;; emacs offsets start at 1 for some reason
                    (delete-char (- end start)) ;; may be 0
                    (insert text)))))
          ;; (message (format "got something %d to %d => [%s]" start end text))))
          (next-line))
        )
      )
    )
  )

(defun rtags-goto-offset(offset)
  (interactive "NOffset: ")
  (if offset
      (goto-char (+ 1 offset))))

(defun rtags-current-symbol-name (&optional cursorinfo)
  (unless cursorinfo
    (setq cursorinfo (rtags-cursorinfo)))
  (let ((container (string-match "^Container:" cursorinfo))
        (symbolname (string-match "^SymbolName: \\(.*\\)$" cursorinfo)))
    (if (and symbolname (or (not container) (< symbolname container)))
        (match-string 1 cursorinfo))))

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
