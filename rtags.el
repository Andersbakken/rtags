(defgroup rtags nil
  "Minor mode for rtags."
  :group 'tools
  :prefix "rtags-")

(require 'ido)
(defvar rtags-last-buffer nil)
(defvar rtags-path-filter nil)
(defvar rtags-path-filter-regex nil)
(defvar rtags-no-otherbuffer nil)
(defface rtags-path nil "Path" :group 'rtags)
(defface rtags-context nil "Context" :group 'rtags)
(defvar rtags-path-face 'rtags-path "Path part")
(defvar rtags-context-face 'rtags-context "Context part")

(defvar rtags-faces
  '(("^[^ ]*" . rtags-path-face)
    (" .*$" . rtags-context-face))
  )

(defvar rtags-mode-map nil)
;; assign command to keys
(setq rtags-mode-map (make-sparse-keymap))
(define-key rtags-mode-map (kbd "RET") 'rtags-select-other-buffer)
(define-key rtags-mode-map (kbd "ENTER") 'rtags-select-other-buffer)
(define-key rtags-mode-map (kbd "SPC") 'rtags-select)
(define-key rtags-mode-map (kbd "q") 'delete-window)
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
  (if (get-buffer "*RTags*")
      (with-current-buffer "*RTags*"
        (beginning-of-line)
        (cond ((and (= (point) (point-min)) (not next)) t)
              ((and (= (point-at-eol) (point-max)) next) t)
              (next (progn (next-line) (unless (= (point) (point-max)) (rtags-goto-location (buffer-substring (point-at-bol) (point-at-eol))))))
              (t (progn (previous-line) (rtags-goto-location (buffer-substring (point-at-bol) (point-at-eol)))))
              )
        )
    )
  )



(defun rtags-executable-find (exe)
  (let ((result (if rtags-path (concat rtags-path "/" exe) (executable-find exe))))
    (if (file-exists-p result) result nil)))

(defun rtags-call-rc (path &rest arguments)
  (let ((rc (rtags-executable-find "rc")))
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
          (if (and path rtags-auto-update-project)
              (push (concat "--project=" path) arguments))

          (rtags-log (concat rc " " (combine-and-quote-strings arguments)))
          (apply #'call-process rc nil (list t nil) nil arguments)
          (goto-char (point-min))
          (rtags-log (buffer-string))
          (> (point-max) (point-min))))))

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
            (c++-mode)
            (local-set-key "q" 'bury-buffer))
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
          (if (string-match "^\\([^ ]*\\) .*$" line)
              (let ((m nil))
                (setq m (buffer-substring (point-at-bol) (+ (point-at-bol) (match-end 1))))
                (setq projects (add-to-list 'projects m t))
                (setq current m))
            (setq projects (add-to-list 'projects (buffer-substring (point-at-bol) (point-at-eol)))))
          (next-line)))
      )
    (setq project (ido-completing-read
                   (format "RTags select project (current is %s): " current)
                   projects))
    (if project
        (with-temp-buffer
          (rtags-call-rc nil "-w" project)))
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
        (substring match 0 (string-match "[^/]*/?$" match)))))
(defun rtags-default-current-project ()
  (cond
   ((gtags-get-rootpath))
   ((git-root-dir))
   ((rtags-find-ancestor-file-directory "configure"))
   ((rtags-find-ancestor-file-directory "CMakeLists.txt"))
   ((rtags-find-ancestor-file-directory "*.pro"))
   ((rtags-find-ancestor-file-directory "scons.1")) ; Is this the right way to determine this?
   ((rtags-find-ancestor-file-directory "autogen.*"))
   ((rtags-find-ancestor-file-directory "Makefile*"))
   ((rtags-find-ancestor-file-directory "INSTALL*"))
   ((rtags-find-ancestor-file-directory "README*"))
   (t nil)))

(defun rtags-current-symbol (&optional no-symbol-name)
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
    name))

(defun rtags-cursorinfo (&optional location)
  (let ((loc (if location location (rtags-current-location)))
        (path (rtags-path-for-project)))
    (with-temp-buffer
      (rtags-call-rc path "-U" loc)
      (buffer-string))))

(defun rtags-print-cursorinfo (&optional location)
  (interactive)
  (message (rtags-cursorinfo)))


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
    (if (get-buffer "*RTags*")
        (kill-buffer "*RTags*"))
    (with-current-buffer (generate-new-buffer "*RTags*")
      (rtags-call-rc path switch tagname "-l")
      (setq rtags-path-filter nil)
      (rtags-handle-completion-buffer)
      )
    )
  )

(defun rtags-remove-completions-buffer ()
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

; **************************** API *********************************

(defcustom rtags-after-find-file-hook nil
  "Run after rtags has jumped to a location possibly in a new file"
  :group 'rtags
  :type 'hook)

(defcustom rtags-auto-update-project t
  "Auto-update project from current buffer"
  :group 'rtags
  :type 'bool)


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
  (define-key map (kbd "C-x r F") (function rtags-fixit))
  (define-key map (kbd "C-x r C") (function rtags-clear-rdm))
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
  )

(defun rtags-print-current-location ()
  (interactive)
  (message (rtags-current-location)))

(defun rtags-quit-rdm () (interactive)
  (call-process (rtags-executable-find "rc") nil nil nil "--quit-rdm"))

(defun rtags-clear-rdm (&optional dontask) (interactive)
  "Use with care, it will destroy the database without possibility of undoing"
  (if (or dontask (y-or-n-p "This will clear the database. Are you sure?"))
      (call-process (rtags-executable-find "rc") nil nil nil "-C")))

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
          (message (buffer-string))))))

(defun rtags-target (&optional location)
  (let ((path (rtags-path-for-project)))
    (unless location
      (setq location (rtags-current-location)))
    (with-temp-buffer
      (rtags-call-rc path "-N" "-f" location)
      (if (< (point-min) (point-max))
          (buffer-substring (point-min) (- (point-max) 1))
        nil))))

(defalias 'rtags-find-symbol-at-point 'rtags-follow-symbol-at-point)
(defun rtags-find-symbol-at-point (prefix)
  (interactive "P")
  (setq rtags-path-filter (if prefix buffer-file-name nil))
  (rtags-save-location)
  (let ((target (rtags-target)))
    (if target
        (rtags-goto-location target))
    )
  )

(defun rtags-find-references-at-point(prefix)
  (interactive "P")
  (setq rtags-path-filter (if prefix buffer-file-name nil))
  (rtags-save-location)
  (let ((arg (rtags-current-location)))
    (if (get-buffer "*RTags*")
        (kill-buffer "*RTags*"))
    (with-current-buffer (generate-new-buffer "*RTags*")
      (rtags-call-rc nil "-l" "-r" arg)
      (setq rtags-path-filter nil)
      (rtags-handle-completion-buffer))
    )
  )

(defun rtags-find-virtuals-at-point(prefix)
  (interactive "P")
  (setq rtags-path-filter (if prefix buffer-file-name nil))
  (rtags-save-location)
  (let ((arg (rtags-current-location)))
    (if (get-buffer "*RTags*")
        (kill-buffer "*RTags*"))
    (with-current-buffer (generate-new-buffer "*RTags*")
      (rtags-call-rc nil "-k" "-l" "-r" arg)
      (setq rtags-path-filter nil)
      (rtags-handle-completion-buffer))
    )
  )

(defun rtags-find-all-references-at-point(prefix)
  (interactive "P")
  (setq rtags-path-filter (if prefix buffer-file-name nil))
  (rtags-save-location)
  (let ((arg (rtags-current-location)))
    (if (get-buffer "*RTags*")
        (kill-buffer "*RTags*"))
    (with-current-buffer (generate-new-buffer "*RTags*")
      (rtags-call-rc nil "-l" "-E" "-r" arg)
      (rtags-handle-completion-buffer))
    )
  )

(defun rtags-rename-symbol ()
  (interactive)
  (save-some-buffers) ; it all kinda falls apart when buffers are unsaved
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

(defun rtags-find-symbol (prefix)
  (interactive "P")
  (rtags-find-symbols-by-name-internal "Find rsymbol" nil (if prefix buffer-file-name nil)))

(defun rtags-find-references (prefix)
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

(defun rtags-fixit()
  (interactive)
  (if (buffer-modified-p)
      (message "I refuse to modifiy a modified buffer")
    (let ((buffer (current-buffer))
          (path (rtags-path-for-project)))
      (with-temp-buffer
        (rtags-call-rc path  "-x" (buffer-file-name buffer))
        (goto-char (point-min))
        (while (looking-at "^\\([0-9]+\\)-?\\([0-9]+\\)? \\(.*\\)$")
          (let ((from (string-to-int (match-string 1)))
                (len (if (stringp (match-string 2)) (string-to-int (match-string 2)) 0))
                (text (match-string 3)))
            (save-excursion
              (set-buffer buffer)
              (goto-char (+ from 1)) ; emacs offsets start at 1 for some reason
              (delete-char len) ; may be 0
              (insert text)))
          (next-line))))))

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
        (progn
          (setq rtags-diagnostics-process
                (start-process
                 "RTags Diagnostics"
                 buf (rtags-executable-find "rc")
                 "-G"
                 (if rtags-autostart-rdm
                     (if rtags-rdm-log-enabled "--autostart-rdm=-L/tmp/rdm.log" "--autostart-rdm"))))
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
               (switch-to-buffer-other-window "*RTags*")
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
      (rtags-call-rc nil "-P" string)
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
(defun rtags-find-file (&optional tagname)
  (interactive)
  (rtags-save-location)
  (let ((tagname (rtags-current-symbol t)) prompt input offset line column
        (path (rtags-path-for-project)))
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
    (if (get-buffer "*RTags*")
        (kill-buffer "*RTags*"))
    (with-current-buffer (generate-new-buffer "*RTags*")
      (rtags-call-rc path "-K" "-P" tagname)
      (cond (offset (replace-regexp "$" (format ",%d" offset)))
            ((and line column) (replace-regexp "$" (format ":%d:%d" line column)))
            ((and line) (replace-regexp "$" (format ":%d" line)))
            (t nil))
      ;(message (format "Got lines and shit %d\n[%s]" (count-lines (point-min) (point-max)) (buffer-string)))
      (cond ((= (point-min) (point-max)) t)
            ((= (count-lines (point-min) (point-max)) 1) (rtags-select))
            (t (progn
                  (switch-to-buffer-other-window "*RTags*")
                  (shrink-window-if-larger-than-buffer)
                  (rtags-mode)
                  ;; (setq rtags-no-otherbuffer t)
                  )))
      ; Should add support for putting offset in there as well, ignore it on completion and apply it at the end
      )
    )
  )

(defun rtags-goto-offset(offset)
  (interactive "NOffset: ")
  (if offset
      (goto-char (+ 1 offset))))

(defun rtags-current-symbol-name ()
  (let ((cursorinfo (rtags-cursorinfo)))
    (if (string-match "^.*symbolName: \\(.*\\) kind: .*$" cursorinfo)
        (match-string 1 cursorinfo))))

(defun rtags-cursor-extent (&optional location)
  (let ((cursorinfo (rtags-cursorinfo location)))
    (if (string-match "^\\(.*\\),[0-9]+ CursorInfo(\\([0-9]+\\)-\\([0-9]+\\) " cursorinfo)
        (let ((start (+ (string-to-int (match-string 2 cursorinfo)) 1))
              (end (+ (string-to-int (match-string 3 cursorinfo)) 1)))
          (cons start end)))))

(defun rtags-target-content (&optional location)
  (let ((cursorinfo (rtags-cursorinfo (rtags-target location))))
    (if (string-match "^\\(.*\\),[0-9]+ CursorInfo(\\([0-9]+\\)-\\([0-9]+\\) " cursorinfo)
        (let ((file (match-string 1 cursorinfo))
              (start (+ (string-to-int (match-string 2 cursorinfo)) 1))
              (end (+ (string-to-int (match-string 3 cursorinfo)) 1)))
          (with-temp-buffer
            (insert-file-contents file start end)
            (buffer-string))))))

(defun rtags-tooltip ()
  (interactive)
  (let ((cursorinfo (rtags-cursorinfo (rtags-target))))
    (message cursorinfo)
    (if (rtags-target)
        (message (rtags-target)))
    (if (string-match "^\\(.*\\),[0-9]+ CursorInfo(\\([0-9]+\\)-\\([0-9]+\\) " cursorinfo)
        (let ((file (match-string 1 cursorinfo))
              (start (+ (string-to-int (match-string 2 cursorinfo)) 1))
              (end (+ (string-to-int (match-string 3 cursorinfo)) 1))
              (tip))
          (save-excursion
            (find-file file)
            (setq tip (buffer-substring start end))
            tip)
          )
      )
    )
  )

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
