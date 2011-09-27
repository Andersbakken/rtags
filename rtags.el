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

;; (defcustom rtags-enable t
;;   "Whether or rtags is enabled"
;;   :type 'boolean
;;   :group 'rtags)

(defun rtags-goto-symbol-at-point()
  (interactive)
  (let ((bufname (buffer-file-name))
        (line (int-to-string (line-number-at-pos)))
        (column nil))
    (save-excursion
      (if (looking-at "[0-9A-Za-z_]")
          (progn
            (while (and (> (point) 0) (looking-at "[0-9A-Za-z_]"))
              (backward-char))
            (if (not (looking-at "[0-9A-Za-z_]"))
                (forward-char))
            (setq column (int-to-string (- (point) (point-at-bol) -1))))))
    (with-temp-buffer
      ;; (message (executable-find "rc"))
      ;; (message (concat (executable-find "rc") " --follow-symbol " bufname ":" line ":" column))
      (call-process (executable-find "rc") nil t nil "--follow-symbol" (concat bufname ":" line ":" column))
      (string-match "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)" (buffer-string))
      (if (match-beginning 1)
          (progn
            (setq line (string-to-int (match-string 2 (buffer-string))))
            (setq column (string-to-int (match-string 3 (buffer-string))))
            (find-file (match-string 1 (buffer-string)))
            (goto-char (point-min))
            (forward-line (- line 1))
            (forward-char (- column 1)))
        (message "Can't follow symbol"))
      )
    )
  )

(defun rtags-complete (string predicate code)
  (let ((completions))
    (with-temp-buffer
      (call-process "rc" nil t nil "-S" "-n" "-l" string)
      (setq 'completions (split-string (buffer-string)))))
      ;; (all-completion string completions))))
      ;; (cond ((eq code nil)
      ;;        (try-completion string completions predicate))
      ;;       ((eq code t)
      ;;        (all-completions string completions predicate))
      ;;       ((eq code 'lambda)
      ;;        (if (intern-soft string completions) t nil))))))


(defun rtags-find-tag (&optional other-win)
  "Input tag name and move to the definition."
  (interactive)
  (let (tagname prompt input completions)
    (setq tagname (gtags-current-token))
    (if tagname
        (setq prompt (concat "Find tag: (default " tagname ") "))
      (setq prompt "Find tag: "))
    (with-temp-buffer
      (call-process "rc" nil t nil "-S" "-n" "-l" (if tagname tagname ""))
      (setq completions (split-string (buffer-string))))
    
    (setq input (completing-read prompt completions nil nil nil gtags-history-list))
    (if (not (equal "" input))
        (setq tagname input))
    (message input)))
    ;; (gtags-push-context)
    ;; (gtags-goto-tag tagname "" other-win)))

;; (defun rtags-goto-symbol()
;;   (interactive)
;;   (let




;; (defun rtags-autostart-arg()
;;   (if rtags-autostart
;;       "--autostart"
;;     ""))


(provide 'rtags)