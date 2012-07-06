(defgroup rtags nil
  "Minor mode for rtags."
  :group 'tools
  :prefix "rtags-")

(defun rtags-call-rc (&rest arguments)
  (push (if rtags-rdm-log-enabled "--autostart-rdm=-L/tmp/rdm.log" "--autostart-rdm") arguments)
  (rtags-log (concat (executable-find "rc") " " (combine-and-quote-strings arguments)))
  (apply #'call-process (executable-find "rc") nil (list t nil) nil arguments)
  (rtags-log (buffer-string))
  (goto-char (point-min)))

(defun rtags-reparse-file(&optional buffer)
  (interactive)
  (with-temp-buffer
    (rtags-call-rc "-V" (buffer-name buffer)))
    (message (format "Dirtied %s" (buffer-name buffer))
    )
  )

(defun rtags-set-current-project ()
  (interactive)
  (let ((projects nil)
        (project nil)
        (current ""))
    (with-temp-buffer
      (rtags-call-rc "-w")
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
    (setq project (completing-read
                   (format "RTags select project (current is %s): " current)
                   projects
                   nil
                   t
                   (try-completion "" projects)))
    (if project
        (with-temp-buffer
          (rtags-call-rc "-w" project)))
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

;; (defvar rtags-current-project-function rtags-default-current-project)

;; (defun rtags-create-path-filter()
;;   (if (functionp rtags-current-project-function)
;;       (concat "-i" (apply 'rtags-current-project-function))
;;     nil))

(defcustom rtags-after-find-file-hook nil
  "Run after rtags has jumped to a location possibly in a new file"
  :group 'rtags
  :type 'hook)

(defvar rtags-last-buffer nil)
(defvar rtags-path-filter nil)

(defun rtags-current-symbol ()
  (cond
   ((looking-at "[0-9A-Za-z_]")
    (while (and (not (bolp)) (looking-at "[0-9A-Za-z_]"))
      (forward-char -1))
    (if (not (looking-at "[0-9A-Za-z_]")) (forward-char 1)))
   (t
    (while (looking-at "[ \t]")
      (forward-char 1))))
  (if (looking-at "[A-Za-z_][A-Za-z_0-9]*")
      (buffer-substring (match-beginning 0) (match-end 0))
    nil))

(defun rtags-cursorinfo (&optional location)
  (let ((loc (if location location (rtags-current-location))))
    (with-temp-buffer
      (rtags-call-rc "-U" loc)
      (buffer-string))))

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
  (setq rtags-path-filter (rtags-default-current-project))
  (rtags-bookmark-push))

(defun rtags-goto-location(location &optional nobookmark)
  "Go to a location passed in. It can be either: file,12 or file:13:14"
  (cond ((string-match "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)" location)
         (let ((line (string-to-int (match-string 2 location)))
               (column (string-to-int (match-string 3 location))))
           (find-file (match-string 1 location))
           (run-hooks rtags-after-find-file-hook)
           (goto-char (point-min))
           (forward-line (- line 1))
           (forward-char (- column 1))
           (unless nobookmark (rtags-bookmark-push))
           t))
        ((string-match "\\(.*\\),\\([0-9]+\\)" location)
         (let ((offset (string-to-int (match-string 2 location))))
           (find-file (match-string 1 location))
           (run-hooks rtags-after-find-file-hook)
           (goto-char (+ offset 1))
           (unless nobookmark (rtags-bookmark-push))
           t))
        (t nil))
  )



(defun rtags-find-symbols-by-name-internal (p references)
  (rtags-save-location)
  (let ((tagname (rtags-current-symbol))
        (switch (if references "-R" "-F"))
        prompt
        input)
    (if tagname
        (setq prompt (concat p ": (default " tagname ") "))
      (setq prompt (concat p ": ")))
    (setq input (completing-read prompt (function rtags-symbolname-complete) nil nil nil rtags-symbol-history))
    (if (not (equal "" input))
        (setq tagname input))
    (if (get-buffer "*RTags Complete*")
        (kill-buffer "*RTags Complete*"))
    (with-current-buffer (generate-new-buffer "*RTags Complete*")
      (rtags-call-rc switch tagname "-l")
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
    (rtags-call-rc "-P" "-S" string)
    (eval (read (buffer-string)))))

(defun rtags-symbolname-completion-exactmatch (string)
  (with-temp-buffer
    (rtags-call-rc "-N" "-F" string)
    (> (point-max) (point-min))))

(defun rtags-symbolname-complete (string predicate code)
  (if (> (length string) 0)
      (cond ((eq code nil)
             (try-completion string (rtags-symbolname-completion-get string) predicate))
            ((eq code t) (rtags-symbolname-completion-get string))
            ((eq code 'lambda) (rtags-symbolname-completion-exactmatch string)))))

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

(defcustom rtags-edit-hook nil
  "Run before rtags tries to modify a buffer (from rtags-rename)
return t if rtags is allowed to modify this file"
  :group 'rtags
  :type 'hook)

(defcustom rtags-jump-to-first-match t
  "If t, jump to first match"
  :group 'rtags
  :type 'boolean)

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



(defun rtags-enable-standard-keybindings (&optional map)
  (interactive)
  (unless map
    (setq map c-mode-base-map))
  (define-key map (kbd "C-x r .") (function rtags-follow-symbol-at-point))
  (define-key map (kbd "C-x r ,") (function rtags-find-references-at-point))
  (define-key map (kbd "C-x r >") (function rtags-find-symbol))
  (define-key map (kbd "C-x r <") (function rtags-find-references))
  (define-key map (kbd "C-x r [") (function rtags-bookmark-back))
  (define-key map (kbd "C-x r ]") (function rtags-bookmark-forward))
  (define-key map (kbd "C-x r \\") (function rtags-bookmark-current))
  (define-key map (kbd "C-x r F") (function rtags-fixit))
  (define-key map (kbd "C-x r C") (function rtags-clear-rdm))
  (define-key map (kbd "C-x r D") (function rtags-diagnostics))
  (define-key map (kbd "C-x r G") (function rtags-clear-diagnostics))
  (define-key map (kbd "C-x r M") (function rtags-index-project))
  (define-key map (kbd "C-x r p") (function rtags-set-current-project))
  (define-key map (kbd "C-x r R") (function rtags-reparse-file))
  )

(defun rtags-print-current-location ()
  (interactive)
  (message (rtags-current-location)))

(defun rtags-quit-rdm () (interactive)
  (call-process (executable-find "rc") nil nil nil "--quit-rdm"))

(defun rtags-clear-rdm (&optional dontask) (interactive)
  "Use with care, it will destroy the database without possibility of undoing"
  (if (or dontask (y-or-n-p "This will clear the database. Are you sure?"))
      (call-process (executable-find "rc") nil nil nil "-C")))

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
        (rtags-call-rc "-m" makefile))))

(defun rtags-follow-symbol-at-point()
  (interactive)
  (rtags-save-location)
  (let ((arg (rtags-current-location)))
    (with-temp-buffer
      (rtags-call-rc "-N" "-f" arg)
      (if (< (point-min) (point-max))
          (rtags-goto-location (buffer-string)))
      )
    )
  )

(defun rtags-find-references-at-point()
  (interactive)
  (rtags-save-location)
  (let ((arg (rtags-current-location)))
    (if (get-buffer "*RTags Complete*")
        (kill-buffer "*RTags Complete*"))
    (with-current-buffer (generate-new-buffer "*RTags Complete*")
      (rtags-call-rc "-l" "-r" arg)
      (rtags-handle-completion-buffer))
    )
  )


(defun rtags-rename-symbol ()
  (interactive)
  (save-some-buffers) ; it all kinda falls apart when buffers are unsaved
  (let (len file pos replacewith prev (modifications 0) (filesopened 0))
    (save-excursion
      (if (looking-at "[0-9A-Za-z_~#]")
          (progn
            (while (and (> (point) 1) (looking-at "[0-9A-Za-z_~#]"))
              (backward-char))
            (if (not (looking-at "[0-9A-Za-z_~#]"))
                (forward-char))
            (setq file (buffer-file-name (current-buffer)))
            (setq pos (point))
            (while (looking-at "[0-9A-Za-z_~#]")
              (forward-char))
            (setq prev (buffer-substring pos (point)))
            (setq len (- (point) pos))
            (setq replacewith (read-from-minibuffer (format "Replace '%s' with: " prev)))
            (unless (equal replacewith "")
              (with-temp-buffer
                (rtags-call-rc "-E" "-O" "-N" "-r" (format "%s,%d" file pos))
                (while (looking-at "^\\(.*\\),\\([0-9]+\\)$")
                  (message (buffer-substring (point-at-bol) (point-at-eol)))
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

(defun rtags-find-symbol ()
  (interactive)
  (rtags-find-symbols-by-name-internal "Find symbol" nil))

(defun rtags-find-references ()
  (interactive)
  (rtags-find-symbols-by-name-internal "Find references" t))

(defun rtags-fixit()
  (interactive)
  (if (buffer-modified-p)
      (message "I refuse to modifiy a modified buffer")
    (let ((buffer (current-buffer)))
      (with-temp-buffer
        (rtags-call-rc "-x" (buffer-file-name buffer))
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
      (compilation-mode)
      (local-set-key "c" 'rtags-clear-diagnostics))
    (if (or (not rtags-diagnostics-process)
            (eq (process-status rtags-diagnostics-process) 'exit))
        (progn
          (setq rtags-diagnostics-process
                (start-process
                 "RTags Diagnostics"
                 buf (executable-find "rc")
                 "-G"
                 (if rtags-rdm-log-enabled "--autostart-rdm=-L/tmp/rdm.log" "--autostart-rdm")))
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
  (unless buffer
    (setq buffer (current-buffer)))
  (with-temp-buffer
    (rtags-call-rc "-t" (buffer-file-name buffer))
    (goto-char (point-min))
    (looking-at "1")
  )
)

(defun rtags-handle-completion-buffer ()
  (let ((empty (= (point-min) (point-max))))
    (cond (empty t)
          ((= (count-lines (point-min) (point-max)) 1)
           (progn
             (rtags-goto-location (buffer-string))))
          (t (progn
               (switch-to-buffer-other-window "*RTags Complete*")
               (goto-char (point-min))
               (compilation-mode)
               (if rtags-jump-to-first-match
                   (compile-goto-error)))))
    (not empty))
  )

(defun rtags-standard-save-hook ()
  (interactive)
  (if (and (get-buffer "*RTags Diagnostics*") (rtags-is-indexed))
      (rtags-clear-diagnostics))
  t)


(provide 'rtags)

