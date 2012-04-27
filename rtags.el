;; (defun rtags-setup-hooks () (interactive)
;;   (remove-hook 'after-save-hook 'rtags-sync-all-open-files)
;;   (remove-hook 'find-file-hooks 'rtags-sync-all-open-files)
  ;; (add-hook 'after-save-hook 'rtags-sync-all-open-files)
  ;; (add-hook 'find-file-hooks 'rtags-sync-all-open-files)
  ;; )

(defgroup rtags nil
  "Minor mode for rtags."
  :group 'tools
  :prefix "rtags-")

(defcustom rtags-edit-hook nil
  "Run before rtags tries to modify a buffer (from rtags-rename)
return t if rtags is allowed to modify this file"
  :group 'rtags
  :type 'hook)

(defcustom rtags-after-find-file-hook nil
  "Run after rtags has jumped to a location possibly in a new file"
  :group 'rtags
  :type 'hook)

(defvar rtags-last-buffer nil)

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

(defun rtags-current-location ()
  (format "%s,%d" (buffer-file-name) (- (point) 1)))

(defun rtags-print-current-location ()
  (interactive)
  (message (rtags-current-location)))

(defun rtags-build-symbol-name-completions()
  (interactive)
  (if (get-buffer "*RTags SymbolName Completions*")
      (kill-buffer "*RTags SymbolName Completions**"))
  (start-process "RTags SymbolName Completions" (generate-new-buffer "*RTags SymbolName Completions*") "rc" "-S"))

(defun rtags-log (log)
  (save-excursion
    (set-buffer (get-buffer-create "*RTags Log*"))
    (goto-char (point-max))
    (setq buffer-read-only nil)
    (insert "**********************************\n" log "\n")
    (setq buffer-read-only t)
    )
  )

(defun rtags-call-rc (&rest arguments)
  (rtags-log (concat (executable-find "rc") " " (combine-and-quote-strings arguments)))
  (apply #'call-process (executable-find "rc") nil (list t nil) nil arguments)
  (rtags-log (buffer-string))
  (goto-char (point-min)))

(defvar rtags-symbol-history nil)
(defun rtags-goto-line-column(location)
  (string-match "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)" location)
  (if (match-beginning 1)
      (let ((line (string-to-int (match-string 2 location)))
            (column (string-to-int (match-string 3 location))))
        (find-file (match-string 1 location))
        (run-hooks rtags-after-find-file-hook)
        (goto-char (point-min))
        (forward-line (- line 1))
        (forward-char (- column 1)))
    t)
  nil)


(defun rtags-goto-location(location)
  (string-match "\\(.*\\),\\([0-9]+\\)" location)
  (if (match-beginning 1)
      (let ((offset (string-to-int (match-string 2 location))))
        (find-file (match-string 1 location))
        (run-hooks rtags-after-find-file-hook)
        ;; (message (concat "current " (buffer-file-name (current-buffer))
        ;;                  " last " (buffer-file-name rtags-last-buffer)))
        (unless (eq (current-buffer) rtags-last-buffer)
          (progn
            (setq rtags-source-buffer (buffer-file-name rtags-last-buffer))
            (make-local-variable 'rtags-source-buffer)
            ))
        (goto-char (+ offset 1))
        t)
    nil)
  )


(defun rtags-follow-symbol-at-point()
  (interactive)
  (setq rtags-last-buffer (current-buffer))
  (let ((arg (rtags-current-location)))
    (with-temp-buffer
      (rtags-call-rc "-N" "-f" arg)
      (rtags-goto-location (buffer-string))
      )
    )
  )

(defun rtags-find-references-at-point()
  (interactive)
  (setq rtags-last-buffer (current-buffer))
  (let ((arg (rtags-current-location)))
    (if (get-buffer "*RTags Complete*")
        (kill-buffer "*RTags Complete*"))
    (switch-to-buffer (generate-new-buffer "*RTags Complete*"))
    (rtags-call-rc "-l" "-r" arg)
    (cond ((= (point-min) (point-max)) (rtags-remove-completions-buffer))
          ((= (count-lines (point-min) (point-max)) 1) (rtags-goto-line-column (buffer-string)))
          (t (progn (goto-char (point-min)) (compilation-mode))))
    (not (= (point-min) (point-max)))
    )
  )


(defun rtags-rename-symbol ()
  (interactive)
  (let (col line len file replacewith prev (modifications 0) (filesopened 0))
    (save-excursion
      (if (looking-at "[0-9A-Za-z_~#]")
          (progn
            (while (and (> (point) 1) (looking-at "[0-9A-Za-z_~#]"))
              (backward-char))
            (if (not (looking-at "[0-9A-Za-z_~#]"))
                (forward-char))
            (setq col (- (point) (point-at-bol) -1))
            (setq line (line-number-at-pos (- (point) 1)))
            (setq file (buffer-file-name (current-buffer)))
            (let ((tmp (point)))
              (while (looking-at "[0-9A-Za-z_~#]")
                (forward-char))
              (setq prev (buffer-substring tmp (point)))
              (setq len (- (point) tmp)))
            (setq replacewith (read-from-minibuffer (format "Replace '%s' with: " prev)))
            (unless (equal replacewith "")
              (with-temp-buffer
                (rtags-call-rc "--no-context" "--all-references" (format "%s:%d:%d:" file line col))
                (while (looking-at "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):$")
                  (message (buffer-substring (point-at-bol) (point-at-eol)))
                  (message (format "%s %s %s" (match-string 1)
                                   (match-string 2)
                                   (match-string 3)))
                  (let ((fn (match-string 1))
                        (l (string-to-number (match-string 2)))
                        (c (string-to-number (match-string 3)))
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
                                (goto-line l)
                                (forward-char (- c 1))
                                ;; (message (format "file %s line %d col %d len %d replacewith %s pos %d" fn l c len replacewith (point)))
                                (kill-forward-chars len)
                                (insert replacewith)
                                ))
                          )))
                  (next-line))
                )))
        (message (format "Opened %d new files and made %d modifications" filesopened modifications))))))

; (get-file-buffer FILENAME)

(defun rtags-find-symbols-by-name-internal (p switch)
  (setq rtags-last-buffer (current-buffer))
  (let (tagname prompt input completions)
    (setq tagname (rtags-current-symbol))
    (if tagname
        (setq prompt (concat p ": (default " tagname ") "))
      (setq prompt (concat p ": ")))
    ;; (if (get-buffer "*RTags SymbolName Completions*")
    ;;     (message "yes")
    ;;     (save-excursion
    ;;       (set-buffer "*RTags SymbolName Completions*")
    ;;       (setq completions (split-string (buffer-string) "\n" t)))
    (with-temp-buffer
      (rtags-call-rc "-S")
      (setq completions (split-string (buffer-string) "\n" t)))
      ;; (setq completions (split-string "test1" "test1()")))
    (setq input (completing-read prompt completions nil nil nil rtags-symbol-history))
    (if (not (equal "" input))
        (setq tagname input))
    (if (get-buffer "*RTags Complete*")
        (kill-buffer "*RTags Complete*"))
    (switch-to-buffer (generate-new-buffer "*RTags Complete*"))
    (rtags-call-rc switch tagname "-l")
    (cond ((= (point-min) (point-max)) (rtags-remove-completions-buffer))
          ((= (count-lines (point-min) (point-max)) 1) (rtags-goto-line-column (buffer-string)))
          (t (progn (goto-char (point-min)) (compilation-mode))))
    (not (= (point-min) (point-max))))
    )

(defun rtags-find-symbol ()
  (interactive)
  (rtags-find-symbols-by-name-internal "Find symbol" "-F"))

(defun rtags-find-references ()
  (interactive)
  (rtags-find-symbols-by-name-internal "Find references" "-R"))

(defun rtags-remove-completions-buffer ()
  (interactive)
  (kill-buffer (current-buffer))
  (switch-to-buffer rtags-last-buffer))

(provide 'rtags)
