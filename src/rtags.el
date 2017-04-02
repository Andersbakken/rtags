;;; rtags.el --- A front-end for rtags

;; Copyright (C) 2011-2017  Jan Erik Hanssen and Anders Bakken

;; Author: Jan Erik Hanssen <jhanssen@gmail.com>
;;         Anders Bakken <agbakken@gmail.com>
;; URL: http://rtags.net
;; Version: 2.9

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


(defgroup rtags nil
  "Minor mode for RTags."
  :prefix "rtags-"
  :group 'tools
  :link '(url-link :tag "Website" "http://rtags.net"))

(require 'bookmark)
(require 'cc-mode)
(require 'tramp)
(require 'simple)
(if (or (> emacs-major-version 24)
        (< emacs-major-version 23)
        (and (= emacs-major-version 24)
             (>= emacs-minor-version 3)))
    (progn
      (require 'cl-lib)
      (defalias 'defun* 'cl-defun)) ;; cl-lib has own namespace now
  (eval-when-compile
    (require 'cl)))
(require 'compile)
(require 'thingatpt)
(require 'repeat)

;; Hack for `kbd'. `kbd' is a macro in Emacs 23, and probably below.
(eval-and-compile
  (if (< emacs-major-version 24)
      (defalias 'kbd 'read-kbd-macro)))

;; Make the byte-compiler happy.
(declare-function flycheck-buffer "ext:flycheck")
(declare-function yas-expand-snippet "ext:yasnippet" t)
(declare-function popup-tip "ext:popup" t)
(declare-function helm "ext:helm" t)
(declare-function ivy-rtags-read "ext:ivy" t)
(declare-function helm-rtags-get-candidate-line 'rtags (candidate))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst rtags-protocol-version 122)
(defconst rtags-popup-available (require 'popup nil t))
(defconst rtags-supported-major-modes '(c-mode c++-mode objc-mode) "Major modes RTags supports.")
(defconst rtags-verbose-results-delimiter "------------------------------------------")
(defconst rtags-buffer-name "*RTags*")
(defconst rtags-diagnostics-buffer-name "*RTags Diagnostics*")
(defconst rtags-diagnostics-raw-buffer-name " *RTags Raw*")

(defconst rtags-exit-code-success 0)
(defconst rtags-exit-code-general-failure 32)
(defconst rtags-exit-code-network-failure 33)
(defconst rtags-exit-code-timeout-failure 34)
(defconst rtags-exit-code-not-indexed 35)
(defconst rtags-exit-code-connection-failure 36)
(defconst rtags-exit-code-protocol-failure 37)
(defconst rtags-exit-code-argument-parse-error 38)
(defconst rtags-return-value-unexpected-message-error 39)
(defconst rtags-return-value-unknown-message-error 40)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar rtags-path-filter nil)
(defvar rtags-path-filter-regex nil)
(defvar rtags-range-filter nil)
(defvar rtags-mode-hook nil)
(defvar rtags-diagnostics-hook nil)
(defvar rtags-jump-hook nil)
(defvar rtags-diagnostics-suspended nil)
(defvar rtags-taglist-hook nil)
(defvar rtags-path-face 'rtags-path "Path part.")
(defvar rtags-context-face 'rtags-context "Context part.")
(defvar rtags-last-request-not-indexed nil)
(defvar rtags-last-request-not-connected nil)
(defvar rtags-buffer-bookmarks 0)
(defvar rtags-diagnostics-process nil)
(defvar rtags-diagnostics-starting nil)
(defvar rtags-tramp-enabled nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom rtags-enabled t
  "Whether RTags is enabled.  We try to do nothing when it's not."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-use-bookmarks t
  "Whether RTags uses bookmarks for locations."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-find-file-absolute nil
  "Whether `rtags-find-file' shows absolute paths."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-wrap-results t
  "Whether `rtags-next-match'/`rtags-previous-match' wraps around."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-close-taglist-on-focus-lost nil
  "Whether `rtags-taglist' should close when it loses focus."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-close-taglist-on-selection t
  "Whether `rtags-taglist' should close when something is selected."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-follow-symbol-try-harder t
  "Fall back to string-matching, if follow symbol fails."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-reindex-on-save nil
  "Explicitly reindex files on save.
This is only be useful, if your file system watching is not working."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-use-filename-completion t
  "Whether RTags special filename completion is enabled.
Set to nil to enable ido-ubiquitous etc."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-diagnostics-use-pipe t
  "Whether diagnostics should use pipes.
If you're running Emacs in cygwin you might have to set this to nil."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-autostart-diagnostics nil
  "Whether RTags automatically will restart diagnostics."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-spellcheck-enabled t
  "Whether RTags does syntax checking with overlays etc to mark errors, warnings and fixups."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-multiple-targets t
  "Whether RTags will offer multiple choices for `rtags-find-symbol-at-point' when appropriate, warnings and fixups."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-verbose-results nil
  "Print more verbose results buffer."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-sort-references-by-input t
  "Whether RTags sorts the references based on the input to `rtags-find-references'."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-completions-enabled nil
  "Whether completions are enabled."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defun rtags-set-transient-map (map)
  "Set transient MAP."
  (cond ((fboundp 'set-transient-map) (set-transient-map map))
        ((fboundp 'set-temporary-overlay-map) (set-temporary-overlay-map map))
        (t)))

(defvar rtags-periodic-reparse-timer nil)
(defun rtags--update-periodic-reparse-timer ()
  "Update periodic reparse timer."
  (when (and (not rtags-periodic-reparse-timer)
             rtags-periodic-reparse-timeout)
    (setq rtags-periodic-reparse-timer
          (run-with-idle-timer rtags-periodic-reparse-timeout t
                               #'rtags-reparse-file-if-needed nil t))))

(defvar rtags-periodic-reparse-timer nil)
;;;###autoload
(defun rtags-set-periodic-reparse-timeout (time)
  "Set `rtags-periodic-reparse-timeout' to TIME."
  (interactive "P")
  (when rtags-periodic-reparse-timer
    (cancel-timer rtags-periodic-reparse-timer))
  (setq rtags-periodic-reparse-timeout (if time (abs time) time))
  (setq rtags-periodic-reparse-timer nil)
  (rtags--update-periodic-reparse-timer))

(defcustom rtags-periodic-reparse-timeout nil
  "Interval, in seconds, for async idle parsing of unsaved buffers.

nil (Unset) means don't reparse preemptively.

Setting this variable directly has no effect, either set this variable using
the Customize interface, `rtags-set-periodic-reparse-timeout',
`customize-set-variable' or `custom-set-variables'."
  :group 'rtags
  :type '(choice (const :tag "Unset" nil) number)
  :risky nil
  :set (lambda (var val)
         (if (boundp var)
             (rtags-set-periodic-reparse-timeout val)
           (set var val))))

(defcustom rtags-update-current-project-timer-interval .5
  "Interval for update current project timer."
  :group 'rtags
  :type 'number
  :safe 'numberp)

(defcustom rtags-wildcard-symbol-names t
  "Allow use of * and ? to match symbol names."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-tracking nil
  "When on automatically jump to symbol under cursor in *RTags* buffer."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

;; Leveraging rtags-socket-file
;; ----------------------------
;; One approach to handling multiple projects is to have separate rdm tag
;; servers, on per project. Consider a project with many thousands of files
;; (thousands of .so's). The size of the rdm tag database on disk will be
;; several GBs. Each project is in it's own workspace on disk often referred to
;; as a sandbox.  Having one rdm tag server for sandbox helps with scalability.
;; To do this, suppose your sandbox root is SBROOT, you would then:
;;    mkdir $SBROOT/.rtags
;;    rdm --socket-file=$SBROOT/.rtags/rdm_socket \
;;        --data-dir=$SBROOT/.rtags/rtags \
;;        --log-file=$SBROOT/.rtags/rdm.log \
;;        --silent \
;;        --watch-sources-only \
;;        --job-count=$NCORES
;;  then in emacs you need to tell rtags.el how to contact to rdm.  You could
;;  use something like:
;;    (add-hook 'find-file-hook
;;            (lambda ()
;;              (if (= (length rtags-socket-file) 0)
;;                  (let ((sbroot (if (buffer-file-name) (get-sbroot-for-buffer) nil))
;;                        (socket-file nil))
;;                      (when sbroot
;;                        (setq socket-file (concat sbroot "/.rtags/rdm_socket"))
;;                        (if (file-exists-p socket-file)
;;                          (setq rtags-socket-file socket-file)))))))
;;  where you provide the get-sbroot-for-buffer function to look for the
;;  .rtags/rdm_socket base on the file currently being loaded, e.g. it is a C++
;;  file that belongs to the project and contains a .rtags/rdm_socket file.
;;
;;  You may also want to provide something like:
;;      (defun sb-set-active-sbroot-for-rtags ()
;;        "Set the active sandbox for use by RTags."
;;        (interactive)
;;        (let* ((current-sbroot (get-sbroot-for-buffer))
;;               (default-dir (if (and current-sbroot
;;                                     (file-exists-p
;;                                      (concat current-sbroot "/.rtags/rdm_socket")))
;;                                current-sbroot
;;                              (if (> (length rtags-socket-file) 0)
;;                                  (replace-regexp-in-string "/\\.rtags/rdm_socket$" ""
;;                                                            rtags-socket-file)
;;                                nil)))
;;               (sbroot (read-file-name "Sandbox root: " nil default-dir))
;;               (socket-file (concat sbroot "/.rtags/rdm_socket")))
;;          (if (not (file-directory-p sbroot))
;;              (error "Directory, %S, does not exist" sbroot))
;;          (if (not (file-exists-p socket-file))
;;              (error "Sandbox has not been indexed (%S does not exist)."
;;                     socket-file))
;;          (setq rtags-socket-file socket-file)))
;;      (easy-menu-add-item nil '(C++)
;;                        ["Set active sbroot for rtags"
;;                         (sb-set-active-sbroot-for-rtags) t])
;;
;;
(defcustom rtags-socket-file ""
  "Socket file to pass to rc."
  :group 'rtags
  :type 'string
  :safe 'stringp)

(defcustom rtags-find-file-prompt "Find files"
  "What prompt to use for ‘rtags-find-file’."
  :group 'rtags
  :type 'string
  :type 'stringp)

(defcustom rtags-track-container nil
  "When on continually update current container (function/class/namespace)
on intervals."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-error-timer-interval .5
  "Interval for minibuffer error timer."
  :group 'rtags
  :type 'number
  :safe 'numberp)

(defcustom rtags-display-current-error-as-message t
  "Display error under cursor using (message)."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-display-current-error-as-tooltip nil
  "Display error under cursor using `popup-tip' (requires 'popup)."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-display-summary-as-tooltip rtags-popup-available
  "Display help / summary text using `popup-tip' (requires 'popup)."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-tooltips-enabled (and rtags-popup-available t)
  "Display help / summary text when hovering over symbols."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-error-timer-interval .5
  "Interval for minibuffer error timer."
  :group 'rtags
  :type 'number
  :safe 'numberp)

(defcustom rtags-tracking-timer-interval .5
  "Interval for tracking timer."
  :group 'rtags
  :type 'number
  :safe 'numberp)

(defcustom rtags-container-timer-interval .5
  "Interval for container timer."
  :group 'rtags
  :type 'number
  :safe 'numberp)

(defcustom rtags-current-container-hook nil
  "Run after RTags has set the current container."
  :group 'rtags
  :type 'hook)

(defcustom rtags-is-indexable 'rtags-is-indexable-default
  "What function to call for expansions."
  :group 'rtags
  :type 'function)

(defcustom rtags-bury-buffer-function 'rtags-bury-or-delete
  "The function used to bury or kill the current rtags buffer."
  :group 'rtags
  :type '(radio (function-item rtags-bury-or-delete)
                (function-item quit-window)
                (function-item bury-buffer)
                (function :tag "Function")))

(defcustom rtags-after-find-file-hook nil
  "Run after RTags has jumped to a location possibly in a new file."
  :group 'rtags
  :type 'hook)

(defcustom rtags-mode-hook nil
  "Run when `rtags-mode' is started."
  :group 'rtags
  :type 'hook)

(defcustom rtags-diagnostics-hook nil
  "Run after diagnostics have been parsed."
  :group 'rtags
  :type 'hook)

(defcustom rtags-completions-hook nil
  "Run after completions have been parsed."
  :group 'rtags
  :type 'hook)

(defcustom rtags-edit-hook nil
  "Run before RTags tries to modify a buffer (from rtags-rename)
return t if RTags is allowed to modify this file."
  :group 'rtags
  :type 'hook)

(defcustom rtags-jump-to-first-match t
  "If t, jump to first match."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-highlight-current-line t
  "If t, highlight the current line in *RTags* buffer."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-timeout nil
  "Max amount of ms to wait before timing out requests."
  :group 'rtags
  :type '(choice (const :tag "Unset" nil) integer)
  :safe 'integerp)

(defcustom rtags-path nil
  "Path to RTags executables."
  :group 'rtags
  :type '(choice (const :tag "Unset" nil) directory)
  :risky t)

(defcustom rtags-max-bookmark-count 100
  "How many bookmarks to keep on the stack."
  :group 'rtags
  :type 'integer
  :safe 'integerp)

(defcustom rtags-rc-log-enabled nil
  "If t, log rc commands and responses."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-show-containing-function nil
  "If t, pass -o to rc to include containing function."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-enable-unsaved-reparsing nil
  "Whether rtags will reparse unsaved buffers as needed."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-reparse-timeout nil
  "Max number of ms you're willing to wait for a reparse to finish."
  :group 'rtags
  :type '(choice (const :tag "Unset" nil) integer)
  :safe 'integerp)

(defcustom rtags-find-file-case-insensitive nil
  "Treat files case-insensitively."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-symbolnames-case-insensitive nil
  "Treat symbol names case-insensitively."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-find-file-prefer-exact-match t
  "Jump directly to files that exactly match the filename for `rtags-find-file'."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-other-window-window-size-percentage 30
  "Percentage size of other buffer."
  :group 'rtags
  :type 'integer)

(defcustom rtags-split-window-function 'split-window
  "Function to split window.  default is `split-window'."
  :group 'rtags
  :type 'function)

(defcustom rtags-buffer-follows-sandbox-id-match 'ask
  "Tells the way current buffer follows sandbox-id in case match fails at a query to rc/rdm backend.

`nil' perform current query without updating diagnostics buffer.
      Diagnostics will be away from current context.
`ask' ask the user if sandbox should be changed. After 'yes',
      perform the command once more.
`t'   change the sandbox and do the command.

Note: If *RTags Diagnostics* is not running, then the 'match check'
      is not performed, because sandbox tracking is not needed then.
Note: It is recommended to run each sandbox is separate Emacs process."
  :group 'rtags
  :type '(choice (const :tag "Perform query without update" nil)
                 (const :tag "Ask the user" ask)
                 (const :tag "Change sandbox and do command" t))
  :safe 'symbolp)

(defcustom rtags-includes-func 'rtags-dummy-includes-func
  "Function to return flags and include flags for rdm."
  :group 'rtags
  :type 'function)

(defcustom rtags-rdm-includes ""
  "Additional include paths."
  :group 'rtags
  :type 'string
  :safe 'stringp)

(defcustom rtags-process-flags ""
  "Flags for rdm."
  :group 'rtags
  :type 'string
  :safe 'stringp)

(defcustom rtags-rdm-process-use-pipe nil
  "If t, use pipes to communicate with rdm."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-popup-results-buffer t
  "Popup the *RTags* buffer when more than one search result is obtained."
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)

(defcustom rtags-display-result-backend 'default
  "Method to use to diplay RTags results, like references."
  :type '(choice (const :tag "RTags (default)" default)
                 (const :tag "Helm" helm)
                 (const :tag "Ivy" ivy))
  :group 'rtags
  :type 'symbol
  :risky t)

(defcustom rtags-imenu-kind-filter "-references,-vardecl,-parmdecl,-inclusiondirective,-*literal*,-enumconstantdecl,-classdecl-,-structdecl-,-classtemplate-,-statements,-lambdaexpr"
  "Argument passed to --kind-filter for ‘rtags-imenu’."
  :group 'rtags
  :type 'string
  :safe 'stringp)

(defcustom rtags-print-filenames-relative t
  "Print filenames relative to the project root directory.

If non-nil, print filenames relative to the project root directory,
otherwise absolute.

Effected interactive functions:
 - `rtags-find-virtuals-at-point'
 - `rtags-find-references'
 - `rtags-find-references-at-point'
 - `rtags-find-all-references-at-point'
 - `rtags-print-class-hierarchy'"
  :group 'rtags
  :type 'boolean
  :safe 'booleanp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface rtags-path nil "Path" :group 'rtags)
(defface rtags-context nil "Context" :group 'rtags)

(defface rtags-warnline
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "orange"))
    (t
     :underline t :inherit error))
  "Face used for marking error lines."
  :group 'rtags)

(defface rtags-errline
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "red"))
    (t
     :underline t :inherit error))
  "Face used for marking warning lines."
  :group 'rtags)

(defface rtags-fixitline
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "chartreuse3"))
    (t
     :underline t :inherit error))
  "Face used for marking fixit lines."
  :group 'rtags)

(defface rtags-current-line
  '((((class color) (background dark)) (:background "gray19"))
    (((class color) (background light)) (:background "LightGray"))
    (t (:bold t)))
  "Face used for highlighting current line."
  :group 'rtags)

(defface rtags-skippedline
  '((((class color) (background dark)) (:background "gray12"))
    (((class color) (background light)) (:background "light gray")))
  "Face used for marking skipped lines."
  :group 'rtags)

(defface rtags-argument-face
  '((((class color) (background dark)) (:background "blue"))
    (((class color) (background light)) (:background "blue"))
    (t (:bold t)))
  "Face used for marking error lines."
  :group 'rtags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rtags-buffer-file-name (&optional buffer)
  "Return the BUFFER file name."
  (buffer-file-name (or (buffer-base-buffer buffer) buffer)))

(defun rtags-remove (predicate seq &optional not)
  "RTags remove."
  (let ((ret))
    (while seq
      (let ((matched (funcall predicate (car seq))))
        (when (cond ((and matched not))
                    ((and (not matched) (not not)))
                    (t nil))
          (setq ret (append ret (list (car seq)))))
        (setq seq (cdr seq))))
    ret))

(defun rtags-remove-last-if-duplicated (seq)
  "Destroy SEQ."
  (let ((newitem (car (last seq))))
    (when (> (length (member newitem seq)) 1)
      (nbutlast seq 1))
    seq))

(defun rtags-is-indexable-default (buffer)
  "Check whether open file in BUFFER is indexable."
  (let ((filename (rtags-buffer-file-name buffer)))
    (when filename
      (let ((suffix (and (string-match "\.\\([^.]+\\)$" filename) (match-string 1 filename))))
        (or (not suffix)
            (and (member (downcase suffix)
                         (list "cpp" "h" "cc" "c" "cp" "cxx" "m" "mm" "tcc" "txx" "moc" "hxx" "hh" "hpp" "inc"))
                 t))))))

(defun rtags-get-buffer (&optional name)
  "Return *RTags* buffer.

When optional argument NAME is non-nil return buffer with NAME instead
of *RTags* buffer."
  (unless name
    (setq name rtags-buffer-name))
  (when (get-buffer name)
    (kill-buffer name))
  (generate-new-buffer name))

(defvar rtags-previous-window-configuration nil)
(make-variable-buffer-local 'rtags-previous-window-configuration)
(put 'rtags-previous-window-configuration 'permanent-local t)

(defun rtags-switch-to-buffer (buffer-or-name &optional other-window)
  "Switch to buffer.
Switch to BUFFER-OR-NAME, when optional argument OTHER-WINDOW is non-nil,
switch to BUFFER-OR-NAME in other window."
  (let ((conf (current-window-configuration)))
    (if other-window
        (switch-to-buffer-other-window buffer-or-name)
      (switch-to-buffer buffer-or-name))
    (set (make-local-variable 'rtags-previous-window-configuration) conf)))

;; for old emacsen
(defun rtags-string-prefix-p (str1 str2 &optional ignore-case)
  "Return non-nil if STR1 is a prefix of STR2.
If IGNORE-CASE is non-nil, the comparison is done without paying attention
to case differences."
  (eq t (compare-strings str1 nil nil
                         str2 0 (length str1) ignore-case)))

(defun rtags-is-rtags-buffer (&optional buffer)
  "Check if buffer is *RTags* buffer."
  (and (not (buffer-file-name buffer))
       (rtags-string-prefix-p "*RTags" (buffer-name buffer))))

(defun rtags-has-diagnostics ()
  "Check for diagnostics."
  (and (get-buffer rtags-diagnostics-buffer-name)
       rtags-diagnostics-process
       (not (eq (process-status rtags-diagnostics-process) 'exit))
       (not (eq (process-status rtags-diagnostics-process) 'signal))
       (> (process-id rtags-diagnostics-process) 0)))

(defun rtags-bury-or-delete ()
  "Bury or delete buffer."
  (interactive)
  (let ((conf rtags-previous-window-configuration)
        (frame (selected-frame)))
    (quit-window nil (selected-window))
    (when (and conf (equal frame (window-configuration-frame conf)))
      (set-window-configuration conf))))

;;;###autoload
(defun rtags-call-bury-or-delete ()
  "Call `rtags-bury-buffer-function' function."
  (interactive)
  (funcall rtags-bury-buffer-function))

(defvar rtags-mode-map nil)
;; assign command to keys
(setq rtags-mode-map (make-sparse-keymap))
(define-key rtags-mode-map (kbd "RET") 'rtags-select-other-window)
(define-key rtags-mode-map (kbd "M-RET") 'rtags-select)
(define-key rtags-mode-map [mouse-1] 'rtags-select-other-window)
(define-key rtags-mode-map [mouse-2] 'rtags-select-other-window)
(define-key rtags-mode-map (kbd "M-o") 'rtags-show-in-other-window)
(define-key rtags-mode-map (kbd "c") 'rtags-select-caller)
(define-key rtags-mode-map (kbd "M-c") 'rtags-select-caller-other-window)
(define-key rtags-mode-map (kbd "s") 'rtags-show-in-other-window)
(define-key rtags-mode-map (kbd "SPC") 'rtags-select-and-remove-rtags-buffer)
(define-key rtags-mode-map (kbd "q") 'rtags-call-bury-or-delete)
(define-key rtags-mode-map (kbd "j") 'next-line)
(define-key rtags-mode-map (kbd "k") 'previous-line)
(define-key rtags-mode-map (kbd "n") 'next-line)
(define-key rtags-mode-map (kbd "p") 'previous-line)

(defvar rtags-dependency-tree-mode-map nil)
(setq rtags-dependency-tree-mode-map (make-sparse-keymap))
(define-key rtags-dependency-tree-mode-map (kbd "TAB") 'rtags-dependency-tree-toggle-current-expanded)
(define-key rtags-dependency-tree-mode-map (kbd "e") 'rtags-dependency-tree-expand-all)
(define-key rtags-dependency-tree-mode-map (kbd "c") 'rtags-dependency-tree-collapse-all)
(define-key rtags-dependency-tree-mode-map (kbd "-") 'rtags-dependency-tree-collapse-current)
(define-key rtags-dependency-tree-mode-map (kbd "+") 'rtags-dependency-tree-expand-current)
(define-key rtags-dependency-tree-mode-map (kbd "P") 'rtags-dependency-tree-find-path)
(define-key rtags-dependency-tree-mode-map (kbd "f") 'rtags-dependency-tree-find-path)
(define-key rtags-dependency-tree-mode-map (kbd "n") 'rtags-dependency-tree-next-level)
(define-key rtags-dependency-tree-mode-map (kbd "p") 'rtags-dependency-tree-previous-level)
(define-key rtags-dependency-tree-mode-map (kbd "RET") 'rtags-select-other-window)
(define-key rtags-dependency-tree-mode-map (kbd "M-RET") 'rtags-select)
(define-key rtags-dependency-tree-mode-map [mouse-1] 'rtags-select-other-window)
(define-key rtags-dependency-tree-mode-map [mouse-2] 'rtags-select-other-window)
(define-key rtags-dependency-tree-mode-map (kbd "M-o") 'rtags-show-in-other-window)
(define-key rtags-dependency-tree-mode-map (kbd "s") 'rtags-show-in-other-window)
(define-key rtags-dependency-tree-mode-map (kbd "SPC") 'rtags-select-and-remove-rtags-buffer)
(define-key rtags-dependency-tree-mode-map (kbd "k") 'previous-line)
(define-key rtags-dependency-tree-mode-map (kbd "j") 'next-line)
(define-key rtags-dependency-tree-mode-map (kbd "q") 'rtags-call-bury-or-delete)

(defvar rtags-references-tree-mode-map nil)
(setq rtags-references-tree-mode-map (make-sparse-keymap))
(define-key rtags-references-tree-mode-map (kbd "TAB") 'rtags-references-tree-toggle-current-expanded)
(define-key rtags-references-tree-mode-map (kbd "e") 'rtags-references-tree-expand-all)
(define-key rtags-references-tree-mode-map (kbd "c") 'rtags-references-tree-collapse-all)
(define-key rtags-references-tree-mode-map (kbd "-") 'rtags-references-tree-collapse-current)
(define-key rtags-references-tree-mode-map (kbd "+") 'rtags-references-tree-expand-current)
(define-key rtags-references-tree-mode-map (kbd "n") 'rtags-references-tree-next-level)
(define-key rtags-references-tree-mode-map (kbd "p") 'rtags-references-tree-previous-level)
(define-key rtags-references-tree-mode-map (kbd "RET") 'rtags-select-other-window)
(define-key rtags-references-tree-mode-map (kbd "M-RET") 'rtags-select)
(define-key rtags-references-tree-mode-map [mouse-1] 'rtags-select-other-window)
(define-key rtags-references-tree-mode-map [mouse-2] 'rtags-select-other-window)
(define-key rtags-references-tree-mode-map (kbd "M-o") 'rtags-show-in-other-window)
(define-key rtags-references-tree-mode-map (kbd "s") 'rtags-show-in-other-window)
(define-key rtags-references-tree-mode-map (kbd "SPC") 'rtags-select-and-remove-rtags-buffer)
(define-key rtags-references-tree-mode-map (kbd "k") 'previous-line)
(define-key rtags-references-tree-mode-map (kbd "j") 'next-line)
(define-key rtags-references-tree-mode-map (kbd "q") 'rtags-call-bury-or-delete)

(defvar rtags-location-stack-visualize-mode-map nil)
(setq rtags-location-stack-visualize-mode-map (make-sparse-keymap))
(define-key rtags-location-stack-visualize-mode-map (kbd "RET") 'rtags-select-other-window)
(define-key rtags-location-stack-visualize-mode-map (kbd "M-RET") 'rtags-select)
(define-key rtags-location-stack-visualize-mode-map [mouse-1] 'rtags-select-other-window)
(define-key rtags-location-stack-visualize-mode-map [mouse-2] 'rtags-select-other-window)
(define-key rtags-location-stack-visualize-mode-map (kbd "M-o") 'rtags-show-in-other-window)
(define-key rtags-location-stack-visualize-mode-map (kbd "s") 'rtags-show-in-other-window)
(define-key rtags-location-stack-visualize-mode-map (kbd "SPC") 'rtags-select-and-remove-rtags-buffer)
(define-key rtags-location-stack-visualize-mode-map (kbd "k") 'previous-line)
(define-key rtags-location-stack-visualize-mode-map (kbd "j") 'next-line)
(define-key rtags-location-stack-visualize-mode-map (kbd "q") 'rtags-call-bury-or-delete)


(defvar rtags-current-file nil)
(make-variable-buffer-local 'rtags-current-file)
(defvar rtags-current-project nil)
(make-variable-buffer-local 'rtags-current-project)

(defconst rtags-c++-keywords '("alignas" "alignof" "and" "and_eq" "asm" "bitand" "bitor"
                               "break" "case" "catch" "class" "compl" "const" "constexpr" "const_cast"
                               "continue" "decltype" "default" "delete" "do" "double" "dynamic_cast"
                               "else" "enum" "explicit" "export" "extern" "false" "float" "for" "friend"
                               "goto" "if" "inline" "mutable" "namespace" "new" "noexcept" "not"
                               "not_eq" "nullptr" "operator" "or" "or_eq" "private" "protected" "public"
                               "register" "reinterpret_cast" "return" "sizeof" "static" "static_assert"
                               "static_cast" "struct" "switch" "template" "this" "thread_local" "throw"
                               "true" "try" "typedef" "typeid" "typename" "union" "using" "virtual"
                               "void" "volatile" "while" "xor" "xor_eq"))

(defconst rtags-c++-templates '("list" "vector" "map" "set" "unordered_map" "multiset" "multimap"
                                "shared_ptr" "weak_ptr" "unique_ptr" "unique_lock"))
(defconst rtags-c++-types '("char" "double" "float" "int" "long" "short" "signed" "unsigned"
                            "void" "u?int[0-9]+_t" "bool" "wchar_t" "std::string" "std::mutex"))

(defvar rtags-font-lock-keywords
  `((,"^\\(.*?:[0-9]+:[0-9]+:\\).*$"
     (1 font-lock-string-face))
    (,"^\\([A-Za-z0-9/._-]*\\)$"
     (1 font-lock-string-face))
    ;; (,(concat "^" rtags-verbose-results-delimiter "$")
    ;;  (1 font-lock-builtin-face))
    (,"^[ \t]+\\(.*\\)$"
     (1 font-lock-function-name-face))))

(defvar rtags-current-line-overlay nil)
(defun rtags-update-current-line ()
  (when (overlayp rtags-current-line-overlay)
    (move-overlay rtags-current-line-overlay (point-at-bol) (point-at-eol))))

(defun rtags-init-current-line-overlay ()
  (when rtags-highlight-current-line
    (let ((overlay (make-overlay (point-at-bol) (point-at-eol) (current-buffer))))
      (overlay-put overlay 'face 'rtags-current-line)
      (set (make-local-variable 'rtags-current-line-overlay) overlay))))

(define-derived-mode rtags-mode fundamental-mode "rtags"
  (set (make-local-variable 'font-lock-defaults)
       '(rtags-font-lock-keywords (save-excursion
                                    (goto-char (point-min))
                                    (when (search-forward "'\"'" nil t)
                                      t))))
  (set (make-local-variable 'rtags-current-file) nil)
  (set (make-local-variable 'rtags-current-project) nil)
  (goto-char (point-min))
  (setq next-error-function 'rtags-next-prev-match)
  (rtags-init-current-line-overlay)
  (setq buffer-read-only t))

(defun rtags-wrap-word (word)
  (concat "[^A-Za-z0-9_]\\(" word "\\)[^A-Za-z0-9_]"))

(font-lock-add-keywords 'rtags-mode
                        (mapcar (lambda (keyword)
                                  (cons (rtags-wrap-word keyword) 'font-lock-keyword-face))
                                rtags-c++-keywords))

(defun rtags-make-type (type) (cons (rtags-wrap-word type) 'font-lock-type-face))
(font-lock-add-keywords 'rtags-mode
                        (mapcar #'rtags-make-type rtags-c++-types))
(font-lock-add-keywords 'rtags-mode
                        (mapcar #'rtags-make-type
                                (let ((ret)
                                      (templates rtags-c++-templates))
                                  (while templates
                                    (let ((template (car templates)))
                                      (push (concat "std::" template " *<[^<>]*<[^<>]*<[^<>]*>[^>]*>[^>]*>") ret)
                                      (push (concat "std::" template " *<[^<>]*<[^<>]*>[^>]*>") ret)
                                      (push (concat "std::" template " *<[^<>]*>") ret))
                                    (setq templates (cdr templates)))
                                  ret)))

(define-derived-mode rtags-dependency-tree-mode fundamental-mode "rtags-dependency-tree-mode"
  ;; (set (make-local-variable 'font-lock-defaults) '(rtags-font-lock-keywords))
  (goto-char (point-min))
  (setq buffer-read-only t))

(define-derived-mode rtags-references-tree-mode fundamental-mode "rtags-references-tree-mode"
  (set (make-local-variable 'font-lock-defaults) '(rtags-font-lock-keywords))
  (goto-char (point-min))
  (rtags-init-current-line-overlay)
  (setq buffer-read-only t))

(defun rtags-bookmark-all-names ()
  (condition-case nil
      (bookmark-all-names)
    (error
     nil)))

(defun rtags-bookmark-set (name)
  (condition-case nil
      (progn (bookmark-set name) t)
    (error
     nil)))

(defun rtags-reset-bookmarks ()
  (setq rtags-buffer-bookmarks 0)
  (let ((bookmark-save-flag t))
    (mapcar (lambda (bookmark)
              (when (string-match "^RTags_" bookmark) (bookmark-delete bookmark)))
            (rtags-bookmark-all-names))))

;;;###autoload
(defun rtags-next-match () (interactive) (rtags-next-prev-match 1 nil))
;;;###autoload
(defun rtags-previous-match () (interactive) (rtags-next-prev-match -1 nil))

(defun rtags-next-prev-match (by reset)
  (when (get-buffer rtags-buffer-name)
    (let ((target)
          (next (> by 0))
          (win (get-buffer-window rtags-buffer-name)))
      (when win
        (select-window win))
      (set-buffer rtags-buffer-name)
      (when reset
        (goto-char (point-min)))
      (when (and (eq rtags-display-result-backend 'helm)
                 (boundp 'helm-action-buffer)
                 (get-buffer-window helm-action-buffer 'visible)
                 (fboundp 'helm-keyboard-quit))
        (helm-keyboard-quit))
      (when (> (count-lines (point-max) (point-min)) 1)
        (while (not (eq by 0))
          (let ((match (save-excursion
                         (if next
                             (and (goto-char (point-at-eol))
                                  (re-search-forward "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\)" nil t))
                           (and (goto-char (point-at-bol))
                                (re-search-backward "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\)" nil t))))))
            (when (cond (match (goto-char match))
                        ((and rtags-wrap-results next)
                         (goto-char (point-min))
                         (re-search-forward "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\)" nil t)
                         (message "%s Wrapped" rtags-buffer-name))
                        ((and rtags-wrap-results (not next))
                         (goto-char (point-max))
                         (re-search-backward "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\)" nil t)
                         (message "%s Wrapped" rtags-buffer-name))
                        (t nil))
              (beginning-of-line)))
          (if next
              (decf by)
            (incf by)))
        (when rtags-highlight-current-line
          (rtags-update-current-line))
        (if win
            (rtags-select-other-window)
          (rtags-select))))))

;;;###autoload
(defun rtags-next-diag () (interactive) (rtags-next-prev-diag t))
;;;###autoload
(defun rtags-previous-diag () (interactive) (rtags-next-prev-diag nil))

(defun rtags-next-prev-diag (next)
  (when (get-buffer rtags-diagnostics-buffer-name)
    (let (target
          (win (get-buffer-window rtags-diagnostics-buffer-name)))
      (when win
        (select-window win))
      (set-buffer rtags-diagnostics-buffer-name)
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
        (if win
            (rtags-select-other-window)
          (rtags-select))))))

(defun rtags-executable-find (exe)
  (cond ((and rtags-tramp-enabled (tramp-tramp-file-p default-directory)) exe)
        ;; for tramp let's rely on `tramp-remote-path`, so if You have some *debug*
        ;; directory to store RTags binaries, just put it to `tramp-remote-path`
        (rtags-path
         (or
          (let ((file (expand-file-name exe rtags-path)))
            (and (file-executable-p file) file))
          (let ((file (expand-file-name exe (concat rtags-path "/bin/"))))
            (and (file-executable-p file) file))))
        (t (executable-find exe))))

(defun rtags-remove-keyword-params (seq)
  (when seq
    (let ((head (car seq))
          (tail (cdr seq)))
      (if (keywordp head)
          (rtags-remove-keyword-params (cdr tail))
        (cons head (rtags-remove-keyword-params tail))))))

(defun rtags-combine-strings (list)
  (mapconcat (lambda (str)
               (cond ((string-match "\"" str) (concat "\"" (replace-regexp-in-string "\"" "\\\"" str) "\""))
                     ((string-match " " str) (concat "\"" str "\""))
                     (t str)))
             list
             " "))

(defun rtags-diagnostics-is-running ()
  (and rtags-diagnostics-process
       (not (eq (process-status rtags-diagnostics-process) 'exit))
       (not (eq (process-status rtags-diagnostics-process) 'signal))))

(defun rtags-get-sandbox-id (path)
  "Returns vector to uniquely define sandbox the path belongs to.
Each host, the emacs is currently connected can be understood as separate sandbox.
nil identifies the local (non-tramp)"
  (if (not (tramp-tramp-file-p path))
      nil
    (let ((path-vec (tramp-dissect-file-name path)))
      (aset path-vec 3 nil)
      path-vec)))

(defun rtags-sandbox-id-matches ()
  "Returns true if current buffer is within *current* sandbox.
*RTags Diagnostics* buffer's sandbox is the *current* sandbox.
If *RTags Diagnostics* does not exist, then t is returned (ie. match for everyone)
Additionally for debugging purposes this method handles `rtags-tramp-enabled` fuse"
  (let (sandbox-match)
    (if (and (tramp-tramp-file-p default-directory) (not rtags-tramp-enabled))
        (message "RTags @ remote site functionality disabled")
      (if (not (and (rtags-diagnostics-is-running) (get-buffer rtags-diagnostics-buffer-name)))
          (setq sandbox-match t)
        (let ((current-buff-sandbox-id (rtags-get-sandbox-id default-directory))
              (buf-name (buffer-name))
              sandbox-id
              diag-start)
          (when (get-buffer rtags-diagnostics-buffer-name)
            (with-current-buffer rtags-diagnostics-buffer-name
              (setq sandbox-id (rtags-get-sandbox-id default-directory))
              (setq sandbox-match (equal current-buff-sandbox-id sandbox-id))
              (when (not sandbox-match)
                (cond ((eq rtags-buffer-follows-sandbox-id-match 'ask)
                       (when (y-or-n-p (format "RTags sandbox mismatch! %s: %s; rtags: %s. Change sandbox?: "
                                               buf-name
                                               (if current-buff-sandbox-id
                                                   (apply 'tramp-make-tramp-file-name (append current-buff-sandbox-id nil))
                                                 "local")
                                               (if sandbox-id
                                                   (apply 'tramp-make-tramp-file-name (append sandbox-id nil))
                                                 "local")))
                         (setq diag-start t)))
                      ((eq rtags-buffer-follows-sandbox-id-match nil)
                       (setq sandbox-match t))
                      (t ;; (eq rtags-buffer-follows-sandbox-id-match t)
                       (setq sandbox-match t
                             diag-start t))))))
          (when diag-start
            (rtags-diagnostics t)
            (message "Done. Issue command once more")))))
    sandbox-match))

(defun rtags-call-process-region
    (start end program &optional delete buffer display &rest args)
  "Use Tramp to handle `call-process-region'.
Fixes a bug in `tramp-handle-call-process-region'.
Function based on org-babel-tramp-handle-call-process-region"
  (if (and (featurep 'tramp)
           rtags-tramp-enabled
           (file-remote-p default-directory))
      (let ((tmpfile (make-temp-file "tramprt")))
        (write-region start end tmpfile)
        (when delete (delete-region start end))
        (unwind-protect
            ;;	(apply 'call-process program tmpfile buffer display args)
            ;; bug in tramp
            (apply 'process-file program tmpfile buffer display args)
          (delete-file tmpfile)))
    (apply 'call-process-region
           start end program delete buffer display args)))

(defun rtags-trampify (absolute-location)
  "if absolute-location is tramped, then return it.
Otherwise if default-directory is tramp one, then uses it to convert
absolute-location to remote. absolute-location can of course be a path"
  (if (or (not rtags-tramp-enabled)
          (not (tramp-tramp-file-p default-directory))
          (tramp-tramp-file-p absolute-location))
      absolute-location
    (let ((location-vec (tramp-dissect-file-name default-directory)))
      (aset location-vec 3 absolute-location)
      (apply 'tramp-make-tramp-file-name (append location-vec nil)))))

(defun rtags-untrampify (location)
  "Gets path segment from tramp path. For non-tramp location just return
it non-modified. Can be used both for path and location."
  (if (tramp-tramp-file-p location)
      (tramp-file-name-localname
       (tramp-dissect-file-name location))
    location))

(defvar rtags--socket-file-cache '("" ""))
(defun rtags--get-socket-file-switch ()
  "Private function which, validates on first access that
`rtags-socket-file' exists and returns
--socket-file=/expanded/path/to/socket/file.  Caller is expected
to only call this when `rtags-socket-file' is defined.
"
  (when (not (string-equal (car rtags--socket-file-cache) rtags-socket-file))
    (setq rtags--socket-file-cache (list rtags-socket-file (expand-file-name rtags-socket-file)))
    (unless (car rtags--socket-file-cache)
      (error "%S does not exist" rtags-socket-file)))

  (concat "--socket-file=" (car rtags--socket-file-cache)))

(defun rtags--convert-output-buffer (arg)
  (cond ((null arg) nil)
        ((eq t arg) (current-buffer))
        ((consp arg) (cons (rtags--convert-output-buffer (car arg))
                           (rtags--convert-output-buffer (cdr arg))))
        (t arg)))

(defun* rtags-call-rc (&rest arguments
                       &key (path (rtags-buffer-file-name))
                       unsaved
                       async ;; nil or a cons (process-filter . sentinel)
                       path-filter
                       path-filter-regex
                       range-filter
                       (output (list t nil)) ; not supported for async
                       range-min
                       range-max
                       noerror
                       timeout
                       silent
                       silent-query
                       &allow-other-keys)
  (save-excursion
    (let ((rc (rtags-executable-find "rc")) result)
      (if (not rc)
          (unless noerror (error "Can't find rc"))
        (setq output (rtags--convert-output-buffer output))
        (setq rtags-last-request-not-connected nil)
        (setq rtags-last-request-not-indexed nil)
        (setq arguments (rtags-remove-keyword-params arguments))
        (setq arguments (rtags-remove '(lambda (arg) (not arg)) arguments))
        (setq arguments (mapcar 'rtags-untrampify arguments))
        ;; other way to ignore colors would IMHO be to configure tramp,
        ;; but: do we need colors from rc?
        (push (format "--verify-version=%d" rtags-protocol-version) arguments)
        (push "-z" arguments)
        (setq path (rtags-untrampify path))
        (when path-filter
          (push (concat "--path-filter=" (rtags-untrampify path-filter)) arguments)
          (when path-filter-regex
            (push "-Z" arguments)))
        (when (and unsaved (rtags-buffer-file-name unsaved))
          (push (format "--unsaved-file=%s:%d"
                        (rtags-untrampify (rtags-buffer-file-name unsaved))
                        (with-current-buffer unsaved
                          (rtags-buffer-size)))
                arguments))
        (when rtags-completions-enabled
          (push "-b" arguments))
        (when silent
          (push "--silent" arguments)
          (setq output nil))
        (when silent-query
          (push "--silent-query" arguments))
        (when range-filter
          (push (format "--range-filter=%d-%d"
                        (or range-min (rtags-offset (point-min)))
                        (or range-max (rtags-offset (point-max))))
                arguments))
        (when (or timeout rtags-timeout)
          (push (format "--timeout=%d" (or timeout rtags-timeout)) arguments))
        (when (and rtags-show-containing-function (not (member "-N" arguments)))
          (push "-o" arguments))

        (cond ((stringp path) (push (concat "--current-file=" path) arguments))
              (path nil)
              (default-directory (push (concat "--current-file=" (rtags-untrampify default-directory)) arguments))
              (t nil))

        (when (> (length rtags-socket-file) 0)
          (push (rtags--get-socket-file-switch) arguments))

        (when rtags-rc-log-enabled
          (rtags-log (concat rc " " (rtags-combine-strings arguments))))
        (let ((result (cond ((and unsaved async)
                             (let ((proc (apply #'start-file-process "rc" (current-buffer) rc arguments)))
                               (with-current-buffer unsaved
                                 (save-restriction
                                   (widen)
                                   (process-send-region proc (point-min) (point-max))))
                               proc))
                            (async (apply #'start-file-process "rc" (current-buffer) rc arguments))
                            ((and unsaved (or (buffer-modified-p unsaved)
                                              (not (rtags-buffer-file-name unsaved))))
                             (with-current-buffer unsaved
                               (save-restriction
                                 (widen)
                                 (apply #'rtags-call-process-region (point-min) (point-max) rc
                                        nil output nil arguments))))
                            (unsaved (apply #'process-file
                                            rc (rtags-untrampify (rtags-buffer-file-name unsaved)) output nil arguments) nil)
                            (t (apply #'process-file rc nil output nil arguments)))))
          (if (processp result)
              (progn
                (set-process-query-on-exit-flag result nil)
                (when (car async)
                  (set-process-filter result (car async)))
                (when (cdr async)
                  (set-process-sentinel result (cdr async))))
            ;; synchronous
            (goto-char (point-min))
            (save-excursion
              (cond ((= result rtags-exit-code-success)
                     (when rtags-autostart-diagnostics
                       (rtags-diagnostics)))
                    ((= result rtags-exit-code-connection-failure)
                     (erase-buffer)
                     (setq rtags-last-request-not-connected t)
                     (unless noerror
                       (error "Can't seem to connect to server. Is rdm running?")))
                    ((= result rtags-exit-code-protocol-failure)
                     (erase-buffer)
                     (unless noerror
                       (error (concat "RTags protocol version mismatch. This is usually caused by getting rtags.el from melpa\n"
                                      "and installing a new rtags build that modified the protocol. They need to be in sync."))))
                    ((= result rtags-exit-code-not-indexed)
                     (erase-buffer)
                     (setq rtags-last-request-not-indexed t))
                    (t)))) ;; other error
          (or async (and (> (point-max) (point-min)) (= result rtags-exit-code-success))))))))

(defvar rtags-preprocess-mode-map (make-sparse-keymap))
(define-key rtags-preprocess-mode-map (kbd "q") 'rtags-call-bury-or-delete)
(set-keymap-parent rtags-preprocess-mode-map c++-mode-map)
(define-derived-mode rtags-preprocess-mode c++-mode "rtags-preprocess"
  ;; Do not run any hooks from `c++-mode', as this could cause issues with, e.g. flycheck
  (set (make-local-variable 'c-mode-common-hook) nil)
  (set (make-local-variable 'c++-mode-hook) nil)
  (when (rtags-buffer-file-name)
    (error "Set buffer with file %s read only " (rtags-buffer-file-name)))
  (setq buffer-read-only t))

(defun rtags-sources (&optional file)
  (with-temp-buffer
    (rtags-call-rc :path file "--sources" file)
    (buffer-string)))

(defmacro rtags-called-interactively-p ()
  (if (or (> emacs-major-version 23)
          (and (>= emacs-major-version 23)
               (>= emacs-minor-version 2)))
      ;; defined with no argument in <=23.1
      `(with-no-warnings (called-interactively-p 'interactive))
    `(interactive-p)))

;;;###autoload
(defun rtags-preprocess-file (&optional buffer)
  "Preprocess selected region or buffer.
If optional BUFFER is given, use BUFFER instead of `current-buffer'.
It uses the stored compile command from the RTags database for preprocessing."
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (unless buffer (setq buffer (current-buffer)))
    (let (narrow-start narrow-end)
      (when (and mark-active
                 (not (equal (region-beginning) (region-end))))
        (setq narrow-start (+ 1 (count-lines (point-min) (region-beginning)))
              narrow-end (+ 1 (count-lines (point-min) (region-end)))))
      (let ((preprocess-buffer (rtags-get-buffer (format "*RTags preprocessed %s*" (rtags-buffer-file-name buffer)))))
        (rtags-delete-rtags-windows)
        (rtags-location-stack-push)
        (with-current-buffer preprocess-buffer
          (rtags-call-rc :path (rtags-buffer-file-name buffer) "--preprocess" (rtags-buffer-file-name buffer))
          (when (and narrow-start narrow-end)
            (let ((match-regexp (concat "^# \\([0-9]*\\) \"" (file-truename (rtags-buffer-file-name buffer)) "\""))
                  last-match last-line start end)
              (while (re-search-forward match-regexp nil t)
                (let ((current-line (string-to-number (match-string-no-properties 1))))
                  (when (and (not start) (> current-line narrow-start))
                    (setq start (+ (count-lines (point-min) last-match) (- narrow-start last-line))))
                  (when (and (not end) (> current-line narrow-end))
                    (setq end (+ (count-lines (point-min) last-match) (- narrow-end last-line))))
                  (setq last-line current-line)
                  (setq last-match (point))))
              (when last-match
                (unless start
                  (setq start (+ (count-lines (point-min) last-match) (- narrow-start last-line))))
                (unless end
                  (setq end (+ (count-lines (point-min) last-match) (- narrow-end last-line)))))
              (when (and start end)
                (goto-char (point-min))
                (narrow-to-region (point-at-bol (+ start 1)) (point-at-bol (+ end 1))))))
          (rtags-preprocess-mode))
        (display-buffer preprocess-buffer)))))

;;;###autoload
(defun rtags-set-current-project ()
  "Set active project.
Uses `completing-read' to ask for the project."
  (interactive)
  (let ((projects nil)
        (project nil)
        (current ""))
    (with-temp-buffer
      (rtags-call-rc :path t "-w")
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
          (cond ((string-match "^\\([^ ]+\\)[^<]*<=$" line)
                 (let ((name (match-string-no-properties 1 line)))
                   (setq projects (add-to-list 'projects name t))
                   (setq current name)))
                ((string-match "^\\([^ ]+\\)[^<]*$" line)
                 (setq projects (add-to-list 'projects (match-string-no-properties 1 line))))
                (t)))
        (forward-line)))
    (setq project (completing-read
                   (format "RTags select project (current is %s): " current)
                   projects))
    (when project
      (find-file project))))

(defun rtags-current-symbol (&optional no-symbol-name)
  (or (and mark-active (buffer-substring-no-properties (point) (mark)))
      (and (not no-symbol-name) (rtags-current-symbol-name))
      (thing-at-point 'symbol)))

(defun* rtags-symbol-info-internal (&rest foo
                                          &key
                                          (parents nil)
                                          (references nil)
                                          (targets nil)
                                          (base-classes nil)
                                          (piece nil)
                                          (relative-filenames nil)
                                          (location (rtags-current-location))
                                          (silent nil))
  (when location
    (let* ((path (rtags-buffer-file-name))
           (object (with-temp-buffer
                     (and location
                          (rtags-call-rc :path path :noerror t :silent-query silent "-U" location "--elisp"
                                         (unless relative-filenames "-K")
                                         (when parents "--symbol-info-include-parents")
                                         (when references "--symbol-info-include-references")
                                         (when targets "--symbol-info-include-targets")
                                         (when base-classes "--symbol-info-include-base-classes"))
                          (goto-char (point-min))
                          (looking-at "(")
                          (eval (read (current-buffer)))))))
      (or (and (not piece) object)
          (cdr (assoc piece object))))))

(defun* rtags-symbol-info (&rest args
                                 &key
                                 (location nil)
                                 (include-targets nil)
                                 (include-references nil)
                                 (include-base-classes nil)
                                 (include-parents nil)
                                 (save-to-kill-ring nil)
                                 (silent-query nil)
                                 (noerror nil)
                                 (no-reparse nil))
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (let ((loc (or location (rtags-current-location)))
          (path (rtags-buffer-file-name)))
      (when (not no-reparse)
        (rtags-reparse-file-if-needed))
      (with-temp-buffer
        (rtags-call-rc :path path
                       :noerror noerror
                       :relative-filenames rtags-print-filenames-relative
                       :silent-query silent-query
                       "-U" loc
                       (when include-targets "--symbol-info-include-targets")
                       (when include-references "--symbol-info-include-references")
                       (when include-base-classes "--symbol-info-include-base-classes")
                       (when include-parents "--symbol-info-include-parents"))
        (when save-to-kill-ring
          (copy-region-as-kill (point-min) (point-max)))
        (when (called-interactively-p 'any)
          (message "%s" (buffer-string)))
        (buffer-string)))))

;;;###autoload
(defun rtags-print-symbol-info (&optional verbose)
  "Print information about the symbol under cursor."
  (interactive "P")
  (message "%s" (rtags-symbol-info :include-parents verbose
                                   :include-targets verbose
                                   :include-references verbose)))

;;;###autoload
(defun rtags-symbol-type ()
  "Print symbol type under cursor."
  (interactive)
  (let* ((info (rtags-symbol-info-internal))
         (type (cdr (assoc 'type info))))
    (when (called-interactively-p 'any)
      (if type
          (message "RTags: %s: %s" (or (cdr (assoc 'symbolName info)) "<unknown>") type)
        (message "RTags: type not found")))
    type))

;; TODO: Can we allow multiple dependency selections, with helm it would be possible?
;; TODO: Briefly describe the last two (depended-on tree-depends-on)..
;;;###autoload
(defun rtags-print-dependencies (&optional prefix buffer)
  "Print dependency information of the file in buffer.

If optional PREFIX is given, a selection of what type of dependency
information should be shown will be offered. Currently only one can
be chosen.
\"includes\"        - Print includes the file in buffer includes.
\"included-by\"     - Print files which include the file in buffer.
\"depends-on\"      - Print files the file in buffer depends on.
\"depended-on\"     - ...
\"tree-depends-on\" - ...

If optional BUFFER is given print dependencies for file in BUFFER
instead of file from `current-buffer'.
"
  (interactive "P")
  (let ((dep-buffer (rtags-get-buffer))
        (fn (rtags-buffer-file-name buffer))
        (args (and prefix
                   (completing-read "Type: " '("includes" "included-by" "depends-on" "depended-on" "tree-depends-on")))))
    (when fn
      (rtags-delete-rtags-windows)
      (rtags-location-stack-push)
      (rtags-switch-to-buffer dep-buffer)
      (rtags-call-rc :path fn "--dependencies" fn args (unless rtags-print-filenames-relative "-K"))
      (rtags-mode))))

;;;###autoload

(defvar rtags-dependency-tree-data nil)
(make-variable-buffer-local 'rtags-dependency-tree-data)

(defvar rtags-tree-indent 2)
(defun rtags-tree-indent (depth)
  (make-string (* depth rtags-tree-indent) ? ))

(defun rtags-dependency-tree-insert-file (file depth)
  (insert (rtags-tree-indent depth) file)
  (let ((count (length (cadr (assoc file rtags-dependency-tree-data)))))
    (when (> count 0)
      (insert " (" (number-to-string count) ")"))))

(defun rtags-dependency-tree-expand-or-collapse-all (expand)
  (save-excursion
    (goto-char (point-min))
    (let (seen)
      (while (not (eobp))
        (let ((current (car (rtags-dependency-tree-current-file))))
          (when current
            (unless (and expand (member current seen))
              (if (not expand)
                  (rtags-dependency-tree-collapse-current)
                (push current seen)
                (rtags-dependency-tree-expand-current)))))
        (forward-line 1)))))

(defun rtags-dependency-tree-expand-all ()
  (interactive)
  (rtags-dependency-tree-expand-or-collapse-all t))

(defun rtags-dependency-tree-collapse-all ()
  (interactive)
  (rtags-dependency-tree-expand-or-collapse-all nil))

(defvar rtags-dependency-tree-matched-decoration " <--")
(defun rtags-dependency-tree-current-file ()
  (save-excursion
    (goto-char (point-at-bol))
    (when (looking-at (concat "^\\( *\\)\\(.*?\\)\\( ([0-9]*)\\)?\\(" rtags-dependency-tree-matched-decoration "\\)?$"))
      (cons (match-string 2) (/ (length (match-string 1)) rtags-tree-indent)))))

(defun rtags-dependency-tree-find-helper (filename)
  (let ((ret)
        (deps rtags-dependency-tree-data))
    (while deps
      (when (member filename (cdar deps))
        (push (caar deps) ret))
      (setq deps (cdr deps)))
    ret))

(defun rtags-dependency-tree-next-level ()
  (interactive)
  (let ((cur (rtags-dependency-tree-current-file)))
    (when cur
      (and (re-search-forward (concat "^" (rtags-tree-indent (1+ (cdr cur))) "[^ ]") nil t)
           (forward-char -1)))))

(defun rtags-dependency-tree-previous-level ()
  (interactive)
  (let ((cur (rtags-dependency-tree-current-file)))
    (when (and cur (> (cdr cur) 0))
      (and (re-search-backward (concat "^" (rtags-tree-indent (1- (cdr cur))) "[^ ]") nil t)
           (skip-chars-forward " ")))))

(defun rtags-dependency-tree-is-visible (filename)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "^ *" (regexp-quote filename) "\\( ([0-9]*)\\)?\\(" rtags-dependency-tree-matched-decoration "\\)?$") nil t)))

(defun rtags-dependency-tree-chains (chain)
  (let ((ret)
        (chains (cddr (assoc (car chain) rtags-dependency-tree-data))))
    (while chains
      (let ((c (append (list (car chains)) chain)))
        (if (rtags-dependency-tree-is-visible (car c))
            (push c ret)
          (let ((subchains (rtags-dependency-tree-chains c)))
            (while subchains
              (push (car subchains) ret)
              (setq subchains (cdr subchains))))))
      (setq chains (cdr chains)))
    ret))

(defun rtags-dependency-tree-find-path (&optional filename)
  (interactive)
  (unless filename
    (setq filename (completing-read "Expand to file: " rtags-dependency-tree-data)))
  (unless filename
    (error "RTags: No file chosen"))
  (rtags-dependency-tree-collapse-all)
  (setq buffer-read-only nil)
  (let ((first)
        (chains (rtags-dependency-tree-chains (list filename))))
    (while chains
      (goto-char (point-min))
      (let* ((chain (car chains))
             (len (1- (length chain)))
             (idx 0))
        (while (< idx len)
          (re-search-forward (concat "^"
                                     (rtags-tree-indent idx)
                                     (regexp-quote (car chain))
                                     "\\( ([0-9]*)\\)?\\("
                                     rtags-dependency-tree-matched-decoration
                                     "\\)?$"))
          ;; (message "EXPANDING %d %s at %d %S" idx (car chain) (point) (rtags-dependency-tree-current-is-expanded))
          (rtags-dependency-tree-expand-current)
          (incf idx)
          (setq chain (cdr chain)))
        (re-search-forward (concat "^" (rtags-tree-indent idx) (regexp-quote (car chain)) "\\( ([0-9]*)\\)?$"))
        (unless (and first (< first (point-at-bol)))
          (setq first (point-at-bol)))
        (unless (eq (char-before) ?*)
          (insert rtags-dependency-tree-matched-decoration)))
      (setq chains (cdr chains)))
    (goto-char first)
    (setq buffer-read-only t)))

(defun rtags-dependency-tree-set-expanded (on)
  (save-excursion
    (let ((was buffer-read-only))
      (setq buffer-read-only nil)
      (let* ((current (rtags-dependency-tree-current-file))
             (children (and current (cadr (assoc (car current) rtags-dependency-tree-data)))))
        (unless current
          (error "RTags no file here"))
        (unless (eq on (null (rtags-dependency-tree-current-is-expanded)))
          (error "RTags line is already %s" (if on "expanded" "collapsed")))
        (set-text-properties (point-at-bol) (point-at-eol) (and on (list 'rtags-is-expanded (length children))))
        (goto-char (point-at-eol))
        (if on
            (progn
              (while children
                (insert "\n")
                (rtags-dependency-tree-insert-file (car children) (1+ (cdr current)))
                (setq children (cdr children))))
          (forward-char 1)
          (let ((start (point))
                (count (length children)))
            (while (> count 0)
              (rtags-dependency-tree-collapse-current)
              (forward-line 1)
              (decf count))
            (delete-region start (point)))))
      (setq buffer-read-only was))))

(defun rtags-dependency-tree-toggle-current-expanded ()
  (interactive)
  (when (rtags-dependency-tree-current-file)
    (rtags-dependency-tree-set-expanded (not (rtags-dependency-tree-current-is-expanded)))))

(defun rtags-dependency-tree-collapse-current ()
  (interactive)
  (when (and (rtags-dependency-tree-current-file) (rtags-dependency-tree-current-is-expanded))
    (rtags-dependency-tree-set-expanded nil)))

(defun rtags-dependency-tree-expand-current ()
  (interactive)
  (when (and (rtags-dependency-tree-current-file) (not (rtags-dependency-tree-current-is-expanded)))
    (rtags-dependency-tree-set-expanded t)))

(defun rtags-dependency-tree-current-is-expanded ()
  (get-text-property (point-at-bol) 'rtags-is-expanded))

(defun rtags-dependency-tree (&optional all)
  (interactive "P")
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (let ((dep-buffer (rtags-get-buffer "*RTags Dependencies*"))
          (deps)
          (fn (rtags-buffer-file-name)))
      (when (or all fn)
        (rtags-delete-rtags-windows)
        (rtags-location-stack-push)
        (with-temp-buffer
          (if all
              (rtags-call-rc :path fn "--elisp" "--all-dependencies" "raw")
            (rtags-call-rc :path fn "--elisp" "--dependencies" fn "raw"))
          (setq deps
                (condition-case nil
                    (eval (read (current-buffer)))
                  (error
                   nil))))
        (rtags-switch-to-buffer dep-buffer)
        (rtags-dependency-tree-mode)
        (setq rtags-dependency-tree-data deps)
        (setq buffer-read-only nil)
        (if (not all)
            (rtags-dependency-tree-insert-file fn 0)
          (while deps
            (rtags-dependency-tree-insert-file (caar deps) 0)
            (insert "\n")
            (setq deps (cdr deps)))
          (delete-char -1))
        (setq buffer-read-only t)
        (unless all
          (rtags-dependency-tree-expand-current))))))

(defun rtags-dependency-tree-all ()
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (rtags-dependency-tree t)))

(defun rtags-references-tree-current-location ()
  (save-excursion
    (goto-char (point-at-bol))
    (and (looking-at "\\( *\\)\\([^ ]+:[0-9]+:[0-9]+:\\)")
         (cons
          (concat rtags-current-project (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
          (/ (length (match-string 1)) rtags-tree-indent)))))

(defun rtags-references-tree-collapse-all ()
  (interactive)
  (goto-char (point-min))
  (save-excursion
    (while (not (eobp))
      (rtags-references-tree-collapse-current)
      (if (= (point-at-eol) (point-max))
          (goto-char (point-max))
        (forward-line 1)))))

(defun rtags-references-tree-expand-all (&optional maxdepth)
  (interactive "p")
  (unless (integerp maxdepth)
    (setq maxdepth nil))
  (rtags-references-tree-collapse-all)
  (goto-char (point-min))
  (let ((seen (make-hash-table :test 'equal)) roots (done 0))
    (save-excursion
      (while (not (eobp))
        (puthash (car (rtags-references-tree-current-location)) t seen)
        (if (= (point-at-eol) (point-max))
            (goto-char (point-max))
          (forward-line 1))))
    (save-excursion
      (setq roots (float (hash-table-count seen)))
      (while (not (eobp))
        (let ((loc (rtags-references-tree-current-location)))
          (cond ((= (cdr loc) 0)
                 (message "Expand all: %g%% %d/%d" (* (/ done roots) 100.0) done roots)
                 (rtags-references-tree-expand-current)
                 (incf done))
                ((not (gethash (car loc) seen))
                 (when (or (not maxdepth) (< (cdr loc) maxdepth))
                   (puthash (car loc) t seen)
                   (rtags-references-tree-expand-current)))
                (t)))
        (if (= (point-at-eol) (point-max))
            (goto-char (point-max))
          (forward-line 1))))
    (message "Expand all: 100%% %d/%d" done roots)))

(defun rtags-file-from-location (location)
  (and location
       (string-match "^\\(.+\\):[0-9]+:[0-9]+:" location)
       (match-string 1 location)))

(defun rtags-references-tree-current-is-expanded ()
  (let ((cur (rtags-references-tree-current-location)))
    (when cur
      (save-excursion
        (forward-line 1)
        (when (not (eobp))
          (let ((next (rtags-references-tree-current-location)))
            (and next (= (1- (cdr next)) (cdr cur)))))))))

(defun rtags-references-tree-set-expanded (on)
  (save-excursion
    (let ((was buffer-read-only))
      (setq buffer-read-only nil)
      (let ((current (rtags-references-tree-current-location))
            (containing-function (get-text-property (point-at-bol) 'rtags-ref-containing-function-location)))
        (unless (and current containing-function)
          (error "RTags no file here"))
        (unless (eq on (null (rtags-references-tree-current-is-expanded)))
          (error "RTags line is already %s" (if on "expanded" "collapsed")))
        (goto-char (point-at-eol))
        (if on
            (let ((refs)
                  (loc (concat rtags-current-project containing-function)))
              (with-temp-buffer
                (rtags-call-rc :path (rtags-file-from-location loc)
                               "-r" loc
                               "--no-sort-references-by-input"
                               "--elisp"
                               "--containing-function-location"
                               "--containing-function")
                (setq refs
                      (condition-case nil
                          (eval (read (current-buffer)))
                        (error
                         nil))))
              (while refs
                (insert "\n")
                (rtags-insert-ref (car refs) (1+ (cdr current)))
                (setq refs (cdr refs)))
              (rtags-references-tree-align-cfs))
          (forward-char 1)
          (let ((start (point)))
            (while (and (not (eobp))
                        (let ((cur (rtags-references-tree-current-location)))
                          (and cur (> (cdr cur) (cdr current)))))
              (forward-line 1))
            (delete-region start (point))))
        (rtags-references-tree-align-cfs)
        (setq buffer-read-only was)))))

(defun rtags-references-tree-toggle-current-expanded ()
  (interactive)
  (let (rtags-references-tree-current-location)
    (rtags-references-tree-set-expanded (not (rtags-references-tree-current-is-expanded)))))

(defun rtags-references-tree-collapse-current ()
  (interactive)
  (when (and (rtags-references-tree-current-location) (rtags-references-tree-current-is-expanded))
    (rtags-references-tree-set-expanded nil)))

(defun rtags-references-tree-expand-current ()
  (interactive)
  (when (and (rtags-references-tree-current-location) (not (rtags-references-tree-current-is-expanded)))
    (rtags-references-tree-set-expanded t)))

(defun rtags-references-tree-next-level ()
  (interactive)
  ;; (let ((cur (rtags-references-tree-current-location)))
  ;;   (when cur
  ;;     (and (re-search-forward (concat "^" (rtags-tree-indent (1+ (cdr cur))) "[^ ]") nil t)
  ;;          (forward-char -1)))))
  )

(defun rtags-references-tree-previous-level ()
  (interactive)
  ;; (let ((cur (rtags-references-tree-current-location)))
  ;;   (when (and cur (> (cdr cur) 0))
  ;;     (and (re-search-backward (concat "^" (rtags-tree-indent (1- (cdr cur))) "[^ ]") nil t)
  ;;          (skip-chars-forward " ")))))
  )

(defun rtags-goto-line-col (line column)
  (let ((old (point))
        (multibyte (rtags-buffer-is-multibyte))
        (prev (buffer-local-value enable-multibyte-characters (current-buffer)))
        (ret t)
        (loc (local-variable-p enable-multibyte-characters)))
    (when multibyte
      (set-buffer-multibyte nil))
    (goto-char (point-min))
    (condition-case nil
        (progn
          (forward-line (1- line))
          (forward-char (1- column)))
      (error
       (setq ret nil)
       (goto-char old)))
    (when multibyte
      (set-buffer-multibyte prev))
    (unless loc
      (kill-local-variable enable-multibyte-characters))
    ret))

(defun rtags-insert-ref (ref level)
  (let* ((location (cdr (assoc 'loc ref)))
         (components
          (and (string-match "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\):?$" location)
               (list (rtags-absolutify (match-string-no-properties 1 location))
                     (string-to-number (match-string-no-properties 2 location))
                     (string-to-number (match-string-no-properties 3 location)))))
         (buffer (and rtags-use-bookmarks (get-file-buffer (car components))))
         (bookmark-idx
          (when buffer
            (let ((deactivate-mark)
                  (bookmark-save-flag t))
              (with-current-buffer buffer
                (save-restriction
                  (widen)
                  (when (rtags-goto-line-col (nth 1 components) (nth 2 components))
                    (when (rtags-bookmark-set (format "RTags_%d" rtags-buffer-bookmarks))
                      (incf rtags-buffer-bookmarks)
                      (1- rtags-buffer-bookmarks)))))))))
    (insert (rtags-tree-indent level) location " " (rtags-format-context (cdr (assoc 'ctx ref)) .4))
    (let ((cf (cdr (assoc 'cf ref)))
          (props (list 'rtags-ref-containing-function-location (cdr (assoc 'cfl ref))))
          (pos (point)))
      (when bookmark-idx
        (setq props (append props (list 'rtags-bookmark-index (cons bookmark-idx (point-at-bol))))))
      (when cf
        (insert " <= " (rtags-elide-text cf (truncate (* (frame-width) .25)) 'right)))

      (set-text-properties (point-at-bol) (point-at-eol) props)
      (when cf
        (set-text-properties pos (point) (append props (list 'rtags-ref-cf t)))))))

(defun rtags-references-tree-align-cfs ()
  (save-excursion
    (goto-char (point-min))
    (let ((longest 0)
          (max)
          (cfs))
      (while (not (eobp))
        (goto-char (point-at-eol))
        (cond ((not (search-backward " <= " (point-at-bol) t))
               (push nil cfs))
              ((not (get-text-property (1+ (point)) 'rtags-ref-cf))
               (push nil cfs))
              (t (push (buffer-substring (point) (point-at-eol)) cfs)
                 (delete-region (point) (point-at-eol))
                 (delete-horizontal-space)))
        (setq longest (max longest (current-column)))
        (or (eobp) (forward-char 1)))
      (goto-char (point-min))
      (setq max (- (frame-width) 2 longest))
      (mapc (lambda (cf)
              (goto-char (point-at-eol))
              (when cf
                (when (> (length cf) max)
                  ;; (message "truncating %s %d vs %d to " cf (length cf) max (substring cf 0 max))
                  (setq cf (substring cf 0 max)))
                (insert (make-string (+ (- longest (current-column))) ? ) cf))
              (unless (eobp)
                (forward-char)))
            (nreverse cfs)))))

;;;###autoload
(defun rtags-references-tree ()
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (rtags-reset-bookmarks)
    (rtags-delete-rtags-windows)
    (let ((ref-buffer (rtags-get-buffer "*RTags*"))
          (loc (rtags-current-location))
          (refs)
          (project)
          (fn (rtags-buffer-file-name)))
      (when (and fn loc)
        (rtags-reparse-file-if-needed)
        (with-temp-buffer
          (rtags-call-rc :path fn
                         "-r" loc
                         "--elisp"
                         "--containing-function-location"
                         "--containing-function"
                         (unless rtags-sort-references-by-input "--no-sort-references-by-input"))
          (setq refs
                (condition-case nil
                    (eval (read (current-buffer)))
                  (error
                   nil))))
        (if (not refs)
            (and (message "RTags: No results") nil)
          (with-temp-buffer
            (rtags-call-rc "--current-project" :path fn)
            (when (> (point-max) (point-min))
              (setq project (buffer-substring-no-properties (point-min) (1- (point-max))))))
          (rtags-delete-rtags-windows)
          (rtags-location-stack-push)
          (rtags-switch-to-buffer ref-buffer)
          (rtags-references-tree-mode)
          (setq rtags-current-project project)
          (setq buffer-read-only nil)
          (mapc (lambda (ref)
                  (rtags-insert-ref ref 0)
                  (insert "\n"))
                refs)
          (rtags-references-tree-align-cfs)
          (delete-char -1)
          (goto-char (point-min))
          (setq buffer-read-only t)
          (cond ((or rtags-last-request-not-indexed rtags-last-request-not-connected) nil)
                ((= (count-lines (point-min) (point-max)) 1)
                 (let ((string (buffer-string)))
                   (rtags-select-and-remove-rtags-buffer)
                   t))
                (rtags-jump-to-first-match
                 (shrink-window-if-larger-than-buffer)
                 (rtags-select-other-window))
                (t
                 (shrink-window-if-larger-than-buffer)
                 t)))))))

(defun rtags-is-function (symbol)
  (member (cdr (assoc 'kind symbol)) (list "CXXMethod" "Constructor" "FunctionDecl" "FunctionTemplate" "Destructor" "LambdaExpr")))

(defun rtags-symbols-in-container (&optional filter)
  (let ((container (rtags-current-container)))
    (and container
         (or (null filter) (funcall filter container))
         (cons container (rtags-symbol-info-internal :targets t
                                                     :location (format "%s:%d:%d:-:%d:%d:"
                                                                       (rtags-buffer-file-name)
                                                                       (cdr (assoc 'startLine container))
                                                                       (cdr (assoc 'startColumn container))
                                                                       (cdr (assoc 'endLine container))
                                                                       (cdr (assoc 'endColumn container))))))))

(defun rtags-find-functions-called-by-this-function-format-reference (file info)
  (let* ((startLine (cdr (assoc 'startLine info)))
         (startColumn (cdr (assoc 'startColumn info)))
         (endLine (cdr (assoc 'startLine info)))
         (endColumn (cdr (assoc 'endColumn info)))
         (contents (cdr (assoc 'contents (rtags-get-file-contents :file file :startLine startLine))))
         (before (1- startColumn))
         (length (length contents))
         (after (and (= startLine endLine) (- length endColumn 1))))
    ;; (when after
    ;;   (setq contents (concat (substring contents 0 before)
    ;;                          (substring contents before (- length 2 after))
    ;;                          ;; (propertize (substring contents before (- length 2 after)) 'face 'rtags-argument-face)
    ;;                          (substring contents after))))
    (and (string-match "^ *\\(.*\\) *$" contents) (match-string 1 contents))))

;;;###autoload
(defun rtags-find-functions-called-by-this-function ()
  (interactive)
  (let* ((containersyms (rtags-symbols-in-container 'rtags-is-function))
         (container (car containersyms))
         (symbols (cdr containersyms))
         (file (rtags-buffer-file-name))
         (calls))
    (while symbols
      (when (and (cdr (assoc 'reference (car symbols))))
        (let ((targets (cdr (assoc 'targets (car symbols)))))
          (while targets
            (when (rtags-is-function (car targets))
              (push (cons (car targets) (car symbols)) calls))
            (setq targets (cdr targets)))))
      (setq symbols (cdr symbols)))
    (if (not calls)
        (message "No function-calls found")
      (with-current-buffer (rtags-get-buffer)
        (while calls
          (goto-char (point-min))
          (let ((call (car calls)))
            (insert (cdr (assoc 'location (car call)))
                    " - "
                    (cdr (assoc 'symbolName (car call)))
                    " - called from - "
                    (format "%s:%d:%d: - "
                            (file-name-nondirectory file)
                            (cdr (assoc 'startLine (cdr call)))
                            (cdr (assoc 'startColumn (cdr call))))
                    (rtags-find-functions-called-by-this-function-format-reference file (cdr call))
                    "\n"))
          (setq calls (cdr calls)))
        (goto-char (point-max))
        (backward-delete-char 1)
        (goto-char (point-min))
        (insert "Functions called from: " (cdr (assoc 'location container)) " " (cdr (assoc 'symbolName container)) "\n")
        (goto-char (point-min))
        (rtags-handle-results-buffer nil nil file)))))

;;;###autoload
(defun rtags-find-all-functions-called-this-function ()



  )


;;;###autoload
(defun rtags-list-results ()
  "Show the RTags results buffer."
  (interactive)
  (rtags-switch-to-buffer rtags-buffer-name t))

;;;###autoload
(defun rtags-print-source-arguments (&optional buffer)
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (let ((args-buffer (rtags-get-buffer))
          (source (rtags-buffer-file-name buffer)))
      (when source
        (rtags-delete-rtags-windows)
        (rtags-location-stack-push)
        (rtags-switch-to-buffer args-buffer)
        (rtags-call-rc :path source "--sources" source)
        (goto-char (point-min))
        (when (= (point-min) (point-max))
          (message "No builds for: %s" source)
          (rtags-location-stack-back)
          (kill-buffer args-buffer))))))

;;;###autoload
(defun rtags-print-class-hierarchy()
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (let ((class-hierarchy-buffer (rtags-get-buffer))
          (path (rtags-buffer-file-name))
          (location (rtags-current-location)))
      (when (and path location)
        (rtags-delete-rtags-windows)
        (rtags-location-stack-push)
        (rtags-switch-to-buffer class-hierarchy-buffer)
        (rtags-call-rc :path path "--class-hierarchy" location (unless rtags-print-filenames-relative "-K"))
        (if (> (point-max) (point-min))
            (rtags-mode)
          (message "No subclasses for: %s" location)
          (rtags-location-stack-back))))))

;;;###autoload
(defun rtags-print-enum-value-at-point (&optional location)
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (let* ((symbol (rtags-symbol-info-internal :location location))
           (enum (or (cdr (assoc 'enumValue symbol))
                     (cdr (assoc 'enumValue (cdr (cadr (assoc 'targets symbol)))))))
           (symbolName (cdr (assoc 'symbolName symbol))))
      (if enum
          (message "RTags: %s - %d - 0x%x" symbolName enum enum)
        (message "RTags: No enum here") nil))))

(defun rtags-buffer-is-multibyte ()
  (string-match "\\butf\\b" (symbol-name buffer-file-coding-system)))

(defun rtags-point-multibyte (&optional p)
  (save-restriction
    (widen)
    (save-excursion
      (when p
        (goto-char p))
      (if (rtags-buffer-is-multibyte)
          (let ((prev (buffer-local-value enable-multibyte-characters (current-buffer)))
                (loc (local-variable-p enable-multibyte-characters))
                (pos))
            (set-buffer-multibyte nil)
            (setq pos (point))
            (set-buffer-multibyte prev)
            (unless loc
              (kill-local-variable enable-multibyte-characters))
            pos)
        (point)))))

(defun rtags-buffer-size ()
  (- (rtags-point-multibyte (point-max)) (point-min)))

(defun rtags-offset (&optional p)
  (1- (rtags-point-multibyte p)))

;;;###autoload
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

(defun rtags-current-location (&optional offset truename)
  (let ((fn (rtags-buffer-file-name)))
    (and fn (format "%s:%d:%d:" (if truename (file-truename fn) fn)
                    (line-number-at-pos offset) (1+ (- (or offset (point)) (point-at-bol)))))))

(defun rtags-log (log)
  (with-current-buffer (rtags-get-buffer-create-no-undo "*RTags Log*")
    (goto-char (point-max))
    (setq buffer-read-only nil)
    (insert (format "**********************************\n%.0f: %s\n" (float-time) log))
    (setq buffer-read-only t)))

(defvar rtags-symbol-history nil)

(defun rtags-find-file-or-buffer (file-or-buffer &optional other-window)
  (if (file-exists-p file-or-buffer)
      (if other-window
          (find-file-other-window file-or-buffer)
        (find-file file-or-buffer))
    (let ((buf (get-buffer file-or-buffer)))
      (if buf(not buf)
        (rtags-switch-to-buffer file-or-buffer other-window)
        (message "No buffer named \"%s\"" file-or-buffer)))))

(defun rtags-absolutify (location &optional skip-trampification)
  (when location
    (save-match-data
      (when (not (string-match "^/" location))
        (unless rtags-current-project
          (let ((file rtags-current-file)
                (project))
            (with-temp-buffer
              (rtags-call-rc :path file "--current-project")
              (when (> (point-max) (point-min))
                (setq project (buffer-substring-no-properties (point-min) (1- (point-max))))))
            (setq rtags-current-project project)))
        (when rtags-current-project
          (setq location (concat rtags-current-project location))))
      (unless (string-match "^/" location)
        (with-temp-buffer
          (rtags-call-rc :path rtags-current-file "--current-project")
          (setq location (concat (buffer-substring-no-properties (point-min) (1- (point-max))) location)))))
    (if skip-trampification
        location
      (rtags-trampify location))))

(defun rtags-goto-location (location &optional nobookmark other-window skip-trampification)
  "Go to a location passed in. It can be either: file,12 or file:13:14 or plain file"
  ;; (message (format "rtags-goto-location \"%s\"" location))
  (setq location (rtags-absolutify location skip-trampification))

  (when (> (length location) 0)
    (cond ((string-match "\\(.*\\) includes /.*" location)
           (rtags-find-file-or-buffer (match-string-no-properties 1 location) other-window)
           (run-hooks 'rtags-after-find-file-hook)
           t)
          ((and (string-match "[^ ]* should include /" location)
                (string= (buffer-substring-no-properties (point-at-bol) (+ (point-at-bol) (length location)))
                         location))
           (save-excursion
             (if (search-backward-regexp "[ (]" (point-at-bol) t)
                 (forward-char 1)
               (goto-char (point-at-bol)))
             (let ((pos (point)))
               (search-forward-regexp " ")
               (rtags-goto-location (buffer-substring-no-properties pos (1- (point))) nobookmark other-window))))
          ((string-match "\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\):?" location)
           (let ((line (string-to-number (match-string-no-properties 2 location)))
                 (column (string-to-number (match-string-no-properties 3 location))))
             (rtags-find-file-or-buffer (match-string-no-properties 1 location) other-window)
             (push-mark nil t)
             (rtags-goto-line-col line column)
             (run-hooks 'rtags-after-find-file-hook)
             t))
          ((string-match "\\(.*?\\):\\([0-9]+\\):?" location)
           (let ((line (string-to-number (match-string-no-properties 2 location))))
             (rtags-find-file-or-buffer (match-string-no-properties 1 location) other-window)
             (push-mark nil t)
             (goto-char (point-min))
             (forward-line (1- line))
             (run-hooks 'rtags-after-find-file-hook)
             t))
          ((string-match "\\(.*?\\),\\([0-9]+\\)" location)
           (let ((offset (string-to-number (match-string-no-properties 2 location))))
             (rtags-find-file-or-buffer (match-string-no-properties 1 location) other-window)
             (push-mark nil t)
             (rtags-goto-offset offset)
             (run-hooks 'rtags-after-find-file-hook)
             t))
          (t
           (when (string-match "^[ \t]+\\(.*\\)$" location)
             (setq location (match-string-no-properties 1 location)))
           (rtags-find-file-or-buffer location other-window)))
    (unless nobookmark (rtags-location-stack-push))))

(defvar rtags-location-stack-index 0)
(defvar rtags-location-stack nil)

(defun rtags-location-stack-push (&optional loc-arg)
  "Push current location into location stack.
If loc-arg is non-nil, then push it instead.
See `rtags-current-location' for loc-arg format."
  (let ((bm (or loc-arg (rtags-current-location))))
    (while (> rtags-location-stack-index 0)
      (decf rtags-location-stack-index)
      (pop rtags-location-stack))
    (unless (string= bm (car rtags-location-stack))
      (push bm rtags-location-stack)
      (when (> (length rtags-location-stack) rtags-max-bookmark-count)
        (nbutlast rtags-location-stack (- (length rtags-location-stack) rtags-max-bookmark-count)))
      (run-hooks 'rtags-jump-hook))))

;;;###autoload
(defun rtags-location-stack-jump (by)
  (interactive)
  (let (;; copy of repeat-on-final-keystroke functionality from repeat.el
        (repeat-char
         (if (eq repeat-on-final-keystroke t)
             last-command-event
           (car (memq last-command-event
                      (listify-key-sequence
                       repeat-on-final-keystroke)))))
        (instack (nth rtags-location-stack-index rtags-location-stack))
        (cur (rtags-current-location)))
    (if (not (string= instack cur))
        ;; location ring may contain locations from many sandboxes. In case current location is remote
        ;; and following is local one, we want following be visited as local file.
        ;; that's why 4th arg is t.
        (rtags-goto-location instack t nil t)
      (let ((target (+ rtags-location-stack-index by)))
        (when (and (>= target 0) (< target (length rtags-location-stack)))
          (setq rtags-location-stack-index target)
          (rtags-goto-location (nth rtags-location-stack-index rtags-location-stack) t nil t))))
    (rtags-location-stack-visualize-update)
    (when repeat-char
      (let ((map (make-sparse-keymap)))
        (define-key map (vector repeat-char)
          `(lambda ()
             (interactive)
             (rtags-location-stack-jump ,by)))
        (rtags-set-transient-map map)))))

(define-derived-mode rtags-location-stack-visualize-mode fundamental-mode "rtags"
  ;; (set (make-local-variable 'font-lock-defaults)
  ;;      '(rtags-font-lock-keywords (save-excursion
  ;;                                   (goto-char (point-min))
  ;;                                   (when (search-forward "'\"'" nil t)
  ;;                                     t))))
  (goto-char (point-min))
  (setq next-error-function 'rtags-next-prev-match)
  (rtags-init-current-line-overlay)
  (setq buffer-read-only t))

;;;###autoload
(defun rtags-location-stack-visualize-update ()
  (let ((buffer (get-buffer "*RTags Location Stack*")))
    (when buffer
      (with-current-buffer buffer
        (let ((idx -1)
              (buffer-read-only nil)
              (lines))
          (erase-buffer)
          (mapc (lambda (entry)
                  (incf idx)
                  (push (if (= idx rtags-location-stack-index)
                            (concat entry " <--")
                          entry) lines))
                rtags-location-stack)
          (insert (mapconcat 'identity lines "\n")))
        (rtags-location-stack-visualize-mode)
        (forward-line rtags-location-stack-index)))))

(defun rtags-location-stack-visualize ()
  (interactive)
  (if (<= (length rtags-location-stack) 1)
      (message "RTags: Location stack is empty")
    (switch-to-buffer (rtags-get-buffer "*RTags Location Stack*"))
    (rtags-location-stack-visualize-update)))

;; **************************** API *********************************

;;;###autoload
(defun rtags-enable-standard-keybindings (&optional map prefix)
  "Setup standard keybindings for the RTags commands.

If optional MAP is non-nil, add the keys to MAP instead of `c-mode-base-map'.
If optional PREFIX is non-nil, use PREFIX as prefix key for the commands,
default is \"C-c r \". It doesn't matter whether you add a space at the end
of PREFIX or not, if doesn't contain one, one will be added."
  (interactive)
  (unless map
    (setq map c-mode-base-map))
  (if prefix
      (unless (string-match " $" prefix)
        (setq prefix (concat prefix " ")))
    (setq prefix "C-c r "))
  (define-key map (kbd (concat prefix ".")) 'rtags-find-symbol-at-point)
  (define-key map (kbd (concat prefix ",")) 'rtags-find-references-at-point)
  (define-key map (kbd (concat prefix "v")) 'rtags-find-virtuals-at-point)
  (define-key map (kbd (concat prefix "V")) 'rtags-print-enum-value-at-point)
  (define-key map (kbd (concat prefix "/")) 'rtags-find-all-references-at-point)
  (define-key map (kbd (concat prefix "Y")) 'rtags-cycle-overlays-on-screen)
  (define-key map (kbd (concat prefix ">")) 'rtags-find-symbol)
  (define-key map (kbd (concat prefix "<")) 'rtags-find-references)
  (define-key map (kbd (concat prefix "[")) 'rtags-location-stack-back)
  (define-key map (kbd (concat prefix "]")) 'rtags-location-stack-forward)
  (define-key map (kbd (concat prefix "D")) 'rtags-diagnostics)
  (define-key map (kbd (concat prefix "C")) 'rtags-compile-file)
  (define-key map (kbd (concat prefix "G")) 'rtags-guess-function-at-point)
  (define-key map (kbd (concat prefix "p")) 'rtags-dependency-tree)
  (define-key map (kbd (concat prefix "P")) 'rtags-dependency-tree-all)
  (define-key map (kbd (concat prefix "e")) 'rtags-reparse-file)
  (define-key map (kbd (concat prefix "E")) 'rtags-preprocess-file)
  (define-key map (kbd (concat prefix "R")) 'rtags-rename-symbol)
  (define-key map (kbd (concat prefix "M")) 'rtags-symbol-info)
  (define-key map (kbd (concat prefix "U")) 'rtags-display-summary-as-message)
  (define-key map (kbd (concat prefix "S")) 'rtags-display-summary)
  (define-key map (kbd (concat prefix "O")) 'rtags-goto-offset)
  (define-key map (kbd (concat prefix ";")) 'rtags-find-file)
  (define-key map (kbd (concat prefix "F")) 'rtags-fixit)
  (define-key map (kbd (concat prefix "L")) 'rtags-copy-and-print-current-location)
  (define-key map (kbd (concat prefix "X")) 'rtags-fix-fixit-at-point)
  (define-key map (kbd (concat prefix "B")) 'rtags-show-rtags-buffer)
  (define-key map (kbd (concat prefix "K")) 'rtags-make-member)
  (define-key map (kbd (concat prefix "I")) 'rtags-imenu)
  (define-key map (kbd (concat prefix "T")) 'rtags-taglist)
  (define-key map (kbd (concat prefix "h")) 'rtags-print-class-hierarchy)
  (define-key map (kbd (concat prefix "a")) 'rtags-print-source-arguments)
  (define-key map (kbd (concat prefix "A")) 'rtags-find-functions-called-by-this-function)
  (define-key map (kbd (concat prefix "l")) 'rtags-list-results)
  (define-key map (kbd (concat prefix "Z")) 'rtags-location-stack-visualize))


;; XXX - would be nice to rename functions to match menu prompts and
;; reorder them to match menu ordering.

(defun rtags-submenu-list (&optional submenu-name)
  "Returns submenu list that can be supplied to `easy-menu-add-item'."
  (if (not submenu-name)
      (setq submenu-name "RTags"))
  (list
   submenu-name
   ["Find symbol definition at point" rtags-find-symbol-at-point]
   ["Find references at point" rtags-find-references-at-point]
   ["Find symbol definition by name" rtags-find-symbol]
   ["Find reference by name" rtags-find-references]
   ["Find all definitions, references, etc. at point"
    rtags-find-all-references-at-point]
   ["Find symbol declaration at point" rtags-guess-function-at-point]
   ["Find virtual method implementations at point" rtags-find-virtuals-at-point]
   ["Find file in RTags database" rtags-find-file]
   ["Print enum value at point" rtags-print-enum-value-at-point]
   "--"
   ["Location stack back" rtags-location-stack-back]
   ["Location stack forward" rtags-location-stack-forward]
   "--"
   ["List all tags for current file" rtags-taglist]
   ["Tags imenu" rtags-imenu]
   ["Print class hierarchy" rtags-print-class-hierarchy]
   "--"
   ["Show compiler diagnostic messages" rtags-diagnostics]
   ["Compile file" rtags-compile-file]
   ["Cycle though diagnostic overlays" rtags-cycle-overlays-on-screen]
   ["Apply compiler fix-it" rtags-fixit]
   ["Apply compiler fix-it at point"  rtags-fixit-at-point]
   "--"
   ["Rename symbol" rtags-rename-symbol]
   ["Make stub member function" rtags-make-member]
   "--"
   ["Display summary of symbol at point in tooltip" rtags-display-summary]
   ["Display summary of symbol at point" rtags-display-summary-as-message]
   ["Display symbol info" rtags-symbol-info]
   ["Print dependencies" rtags-dependency-tree]
   ["Print dependencies for all sources" rtags-dependency-tree-all]
   "--"
   ["Reparse file" rtags-reparse-file]
   ["Preprocess file" rtags-preprocess-file]
   (list
    "RTags Development"
    ["Copy and print current location" rtags-copy-and-print-current-location]
    ["Print source arguments" rtags-print-source-arguments]
    ["Goto offset" rtags-goto-offset]
    ["Show *RTags* buffer" rtags-show-rtags-buffer]
    ["Show *RTags* buffer in other window" rtags-list-results])))

(defun rtags-add-submenu (menu-name)
  "Add the RTags submenu to menu-name")

(add-hook 'c++-mode-hook '(lambda () (easy-menu-add-item nil '("C++") (rtags-submenu-list))))
(add-hook 'c-mode-hook '(lambda () (easy-menu-add-item nil '("C") (rtags-submenu-list))))
(add-hook 'objc-mode-hook '(lambda () (easy-menu-add-item nil '("ObjC") (rtags-submenu-list))))

;;;###autoload
(defun rtags-print-current-location ()
  (interactive)
  (message (rtags-current-location)))

;;;###autoload
(defun rtags-location-stack-forward ()
  (interactive)
  (rtags-location-stack-jump -1))

;;;###autoload
(defun rtags-location-stack-back ()
  (interactive)
  (rtags-location-stack-jump 1))

;;;###autoload
(defun rtags-location-stack-reset ()
  (interactive)
  (setq rtags-location-stack nil)
  (setq rtags-location-stack-index 0))

(defun rtags-target (&optional filter declaration-only no-reparse no-error)
  "DONT-REPARSE : do not reparse file even if it appears as modified."
  (let ((path (rtags-buffer-file-name))
        (location (rtags-current-location))
        (unsaved (and (buffer-modified-p) (current-buffer))))
    (when path
      (unless no-reparse
        (rtags-reparse-file-if-needed))
      (with-temp-buffer
        (if declaration-only
            (rtags-call-rc :path-filter filter :unsaved unsaved :noerror t :path path "-G" "-N" "-f" location "-K")
          (rtags-call-rc :noerror t :unsaved unsaved :path-filter filter :path path "-N" "-f" location "-K"))
        (cond ((= (point-min) (point-max))
               (unless no-error (message "RTags: No target")) nil)
              (rtags-last-request-not-indexed nil)
              (t (buffer-substring-no-properties (point-min) (- (point-max) 1))))))))

(defun rtags-target-declaration-first ()
  "First try to find the declaration of the item (using --declaration-only), then try
to find anything about the item."
  (let ((target (or (rtags-target nil t nil t)
                    (rtags-target nil nil nil t))))
    target))


(defun rtags-buffer-lines () (count-lines (point-min) (point-max)))
;;;###autoload

(defun rtags-find-symbol-at-point (&optional prefix)
  "Find the natural target for the symbol under the cursor and moves to that location.
For references this means to jump to the definition/declaration of the referenced symbol (it jumps to the definition if it is indexed).
For definitions it jumps to the declaration (if there is only one) For declarations it jumps to the definition.
If called with prefix, open first match in other window"
  (interactive "P")
  (let ((otherwindow (and prefix (listp prefix)))
        (pathfilter (and (numberp prefix) (rtags-buffer-file-name))))
    (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
      (rtags-location-stack-push)
      (let ((arg (rtags-current-location))
            (tagname (or (rtags-current-symbol) (rtags-current-token)))
            (fn (rtags-buffer-file-name)))
        (rtags-reparse-file-if-needed)
        (let ((results (with-temp-buffer
                         (rtags-call-rc :path fn :path-filter pathfilter "-f" arg (if rtags-multiple-targets "--all-targets"))
                         (when (and (= (rtags-buffer-lines) 0)
                                    rtags-follow-symbol-try-harder
                                    (> (length tagname) 0))
                           (rtags-call-rc :path-filter pathfilter :path fn "-F" tagname "--definition-only" "-M" "1" "--dependency-filter" fn)
                           (when (= (rtags-buffer-lines) 0)
                             (rtags-call-rc :path fn :path-filter pathfilter "-F" tagname "-M" "1" "--dependency-filter" fn)))
                         (cons (buffer-string) (rtags-buffer-lines)))))
          (cond ((= (cdr results) 0) nil)
                ((= (cdr results) 1)
                 (with-temp-buffer
                   (insert (car results))
                   (goto-char (point-min))
                   (rtags-handle-results-buffer nil nil fn otherwindow)))
                (t
                 (rtags-delete-rtags-windows)
                 (with-current-buffer (rtags-get-buffer)
                   (insert (car results))
                   (goto-char (point-min))
                   (rtags-handle-results-buffer nil nil fn otherwindow)))))))))

;;;###autoload
(defun rtags-find-references-at-point (&optional prefix)
  "Find all references to the symbol under the cursor.

If there's exactly one result jump directly to it, and if optional
PREFIX is given jump to it in other window. If there's more show a
buffer with the different alternatives and jump to the first one, if
`rtags-jump-to-first-match' is true. References to references will be
treated as references to the referenced symbol."
  (interactive "P")
  (let ((otherwindow (and prefix (listp prefix)))
        (pathfilter (and (numberp prefix) (rtags-buffer-file-name))))
    (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
      (rtags-delete-rtags-windows)
      (rtags-location-stack-push)
      (let ((arg (rtags-current-location))
            (fn (rtags-buffer-file-name)))
        (rtags-reparse-file-if-needed)
        (with-current-buffer (rtags-get-buffer)
          (rtags-call-rc :path fn :path-filter pathfilter "-r" arg
                         (unless rtags-sort-references-by-input "--no-sort-references-by-input"))
          (rtags-handle-results-buffer nil nil fn otherwindow))))))

;;;###autoload
(defun rtags-find-virtuals-at-point (&optional prefix)
  "List all reimplentations of function under cursor.
This includes both declarations and definitions."
  (interactive "P")
  (let ((otherwindow (and prefix (listp prefix)))
        (pathfilter (and (numberp prefix) (rtags-buffer-file-name))))
    (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
      (rtags-delete-rtags-windows)
      (rtags-location-stack-push)
      (let ((arg (rtags-current-location))
            (fn (rtags-buffer-file-name)))
        (rtags-reparse-file-if-needed)
        (with-current-buffer (rtags-get-buffer)
          (rtags-call-rc :path fn
                         :path-filter pathfilter
                         "-r" arg
                         "-k"
                         (unless rtags-sort-references-by-input "--no-sort-references-by-input")
                         (unless rtags-print-filenames-relative "-K"))
          (rtags-handle-results-buffer nil nil fn otherwindow))))))

;;;###autoload
(defun rtags-find-all-references-at-point (&optional prefix)
  (interactive "P")
  (let ((otherwindow (and prefix (listp prefix)))
        (pathfilter (and (numberp prefix) (rtags-buffer-file-name))))
    (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
      (rtags-delete-rtags-windows)
      (rtags-location-stack-push)
      (let ((arg (rtags-current-location))
            (fn (rtags-buffer-file-name)))
        (rtags-reparse-file-if-needed)
        (with-current-buffer (rtags-get-buffer)
          (rtags-call-rc :path fn
                         :path-filter pathfilter
                         "-r" arg
                         "-e"
                         (unless rtags-sort-references-by-input "--no-sort-references-by-input")
                         (unless rtags-print-filenames-relative "-K"))
          (rtags-handle-results-buffer nil nil fn otherwindow))))))

;;;###autoload
(defun rtags-guess-function-at-point()
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (rtags-delete-rtags-windows)
    (rtags-location-stack-push)
    (let ((token (rtags-current-token))
          (fn (rtags-buffer-file-name)))
      (when token
        (rtags-reparse-file-if-needed)
        (with-current-buffer (rtags-get-buffer)
          (rtags-call-rc :path fn "-G" "-F" token)
          (rtags-handle-results-buffer t nil fn))))))

(defun rtags-current-token ()
  (save-excursion
    (when (looking-at "[0-9A-Za-z_~#]")
      (while (and (> (point) (point-min)) (looking-at "[0-9A-Za-z_~#]"))
        (backward-char))
      (when (not (looking-at "[0-9A-Za-z_~#]"))
        (forward-char))
      (let ((start (point)))
        (while (looking-at "[0-9A-Za-z_~#]")
          (forward-char))
        (buffer-substring-no-properties start (point))))))

(defun rtags-rename-confirm-text (confirms prevlen)
  (with-temp-buffer
    (let ((lastfile)
          (lastline)
          (offsets))
      (dolist (confirm confirms)
        (let ((file (cdr (assoc 'filename confirm)))
              (line (cdr (assoc 'line confirm))))
          (unless (and (string= lastfile file)
                       (= lastline line))
            (when lastline
              (insert "\n"))
            (insert (cdr (assoc 'contents confirm)))
            (setq lastline line)
            (setq lastfile file))
          (push (point-at-bol) offsets)))
      (setq offsets (nreverse offsets))
      (dolist (confirm confirms)
        (goto-char (+ (car offsets) (cdr (assoc 'col confirm)) -1))
        (setq offsets (cdr offsets))
        (set-text-properties (point) (+ (point) (or prevlen (length (rtags-current-token)))) (list 'face 'rtags-argument-face))))
    (buffer-string)))

;;;###autoload
(defun rtags-rename-symbol (&optional no-confirm)
  (interactive "P")
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (save-some-buffers) ;; it all kinda falls apart when buffers are unsaved
    (let* ((prev (let ((token (rtags-current-token)))
                   (cond ((string-match "^~" token) (substring token 1))
                         (token)
                         (t (error "Not sure what to rename")))))
           (len (and prev (length prev)))
           (file (rtags-buffer-file-name))
           (replacewith (read-from-minibuffer
                         (if len
                             (format "Replace '%s' with: " prev)
                           "Replace with: ")))
           (modifications 0)
           (confirmbuffer (and (not no-confirm) (rtags-get-buffer "*RTags rename symbol*")))
           (filesopened 0)
           (location (rtags-current-location))
           (confirms)
           replacements)
      (save-excursion
        (when (equal replacewith "")
          (error "You have to replace with something"))
        (with-temp-buffer
          (rtags-call-rc :path file "-e" "--rename" "-N" "-r" location "-K")
          ;; (message "Got renames %s" (buffer-string))
          (dolist (string (split-string (buffer-string) "\n" t))
            (when (string-match "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):$" string)
              (let* ((filename (rtags-trampify (match-string-no-properties 1 string)))
                     (line (string-to-number (match-string-no-properties 2 string)))
                     (col (string-to-number (match-string-no-properties 3 string)))
                     (buf (or (find-buffer-visiting filename)
                              (let ((b (find-file-noselect filename)))
                                (and b (incf filesopened) b)))))
                (unless (bufferp buf)
                  (error "Can't open file %s" filename))
                (with-current-buffer buf
                  (save-excursion
                    (rtags-goto-line-col line col)
                    (when (cond ((looking-at prev))
                                ((looking-at (concat "~" prev)) (forward-char) t)
                                ((looking-at "auto ") nil)
                                (t (error "Rename gone awry. Refusing to rename %s (%s) to %s"
                                          (rtags-current-token)
                                          (rtags-current-location)
                                          replacewith)))
                      (when confirmbuffer
                        (push (list (cons 'filename filename)
                                    (cons 'line line)
                                    (cons 'col col)
                                    (cons 'contents (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
                              confirms))
                      (add-to-list 'replacements (cons (current-buffer) (point))))))))))
        (unless no-confirm
          (rtags-switch-to-buffer (rtags-get-buffer "*RTags rename symbol*"))
          (insert (propertize (concat "Change to '" replacewith) 'face 'rtags-context-face) "'\n" (rtags-rename-confirm-text (nreverse confirms) len) "\n")
          (goto-char (point-min))
          (unless (y-or-n-p (format "RTags: Confirm %d renames? " (length confirms)))
            (setq replacements nil))
          (kill-buffer (current-buffer)))
        (dolist (value replacements)
          (with-current-buffer (car value)
            (when (run-hook-with-args-until-failure 'rtags-edit-hook)
              (incf modifications)
              (goto-char (cdr value))
              ;; (message "about to insert at %s" (rtags-current-location))
              (delete-char (or len (length (rtags-current-token))))
              (insert replacewith)
              (basic-save-buffer))))
        (message (format "Opened %d new files and made %d modifications" filesopened modifications))))))

;;;###autoload
(defun rtags-find-symbol (&optional prefix)
  (interactive "P")
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (rtags-find-symbols-by-name-internal "Find rsymbol" "-F" nil nil prefix)))

;;;###autoload
(defun rtags-find-references (&optional prefix)
  (interactive "P")
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (rtags-find-symbols-by-name-internal "Find rreferences" "-R" nil nil prefix)))

;;;###autoload
(defun rtags-find-symbol-current-file ()
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (rtags-find-symbol t)))

;;;###autoload
(defun rtags-find-references-current-file ()
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (rtags-find-references t)))

(defun rtags-dir-filter ()
  (concat (substring buffer-file-name 0 (string-match "[^/]*/?$" buffer-file-name)) "[^/]* "))

;;;###autoload
(defun rtags-find-symbol-current-dir ()
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (rtags-find-symbols-by-name-internal "Find rsymbol" "-F" (rtags-dir-filter) t)))

;;;###autoload
(defun rtags-find-references-current-dir ()
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (rtags-find-symbols-by-name-internal "Find rreferences" (rtags-dir-filter) t)))

;;;###autoload
(defun rtags-apply-fixit-at-point ()
  (interactive)
  (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
    (when (string-match "^\\(.*\\):[0-9]+:[0-9]+: fixit: \\([0-9]+\\)-\\([0-9]+\\): .*did you mean '\\(.*\\)'\\?$" line)
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
            (save-restriction
              (widen)
              (goto-char start)
              (delete-char (- end start)) ;; may be 0
              (insert text))))))))

(defvar rtags-overlays-buffers nil)

(defun rtags-overlays-buffers-add (buffer)
  (add-to-list 'rtags-overlays-buffers buffer))

(defun rtags-overlays-buffers-contains (buffer)
  (member buffer rtags-overlays-buffers))

(defun rtags-overlays-buffers-remove (buffer)
  (setq rtags-overlays-buffers (delq buffer rtags-overlays-buffers)))

(defun rtags-overlays-buffers-set (buffer on)
  (if on
      (rtags-overlays-buffers-add buffer)
    (rtags-overlays-buffers-remove buffer)))

(defun rtags-overlays-remove (&optional no-update-diagnostics-buffer)
  (save-restriction
    (widen)
    (let ((overlays (overlay-lists)))
      (dolist (overlay (car overlays))
        (when (rtags-is-rtags-overlay overlay)
          (delete-overlay overlay)))
      (dolist (overlay (cdr overlays))
        (when (rtags-is-rtags-overlay overlay)
          (delete-overlay overlay)))))
  (unless no-update-diagnostics-buffer
    (let ((diagnostics-buffer (get-buffer rtags-diagnostics-buffer-name))
          (rx (concat "^" (file-truename (rtags-buffer-file-name)) ":")))
      (when diagnostics-buffer
        (with-current-buffer diagnostics-buffer
          (setq buffer-read-only nil)
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (if (looking-at rx)
                  (delete-char (- (1+ (point-at-eol)) (point)))
                (forward-line))))
          (setq buffer-read-only t))))))

;;;###autoload
(defun rtags-clear-diagnostics-overlays (&optional buf)
  (interactive)
  (if buf
      (with-current-buffer buf
        (rtags-overlays-remove))
    (rtags-overlays-remove)))

(defun rtags-clear-all-diagnostics-overlays()
  (interactive)
  (dolist (buf rtags-overlays-buffers)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (rtags-overlays-remove t))))
  (let ((diagnostics-buffer (get-buffer rtags-diagnostics-buffer-name)))
    (when (buffer-live-p diagnostics-buffer)
      (with-current-buffer diagnostics-buffer
        (setq buffer-read-only nil)
        (erase-buffer)
        (setq buffer-read-only t)))))

(defvar rtags-error-warning-count nil)
(make-variable-buffer-local 'rtags-error-warning-count)

(defvar rtags-last-index nil)
(defvar rtags-last-total nil)

(defun rtags-modeline-format-helper (type count)
  (and (> count 0)
       (format "%d %s%s" count type (if (> count 1) "s" ""))))

(defun rtags-modeline()
  (let* ((progress
          (and rtags-last-index
               rtags-last-total
               (> rtags-last-total rtags-last-index)
               (> rtags-last-total 0)
               (format "%d/%d %d%%%%" rtags-last-index rtags-last-total (/ (* rtags-last-index 100) rtags-last-total))))
         (errors (if rtags-error-warning-count
                     (car rtags-error-warning-count)
                   0))
         (warnings (if rtags-error-warning-count
                       (cdr rtags-error-warning-count)
                     0))
         (errorsString (rtags-modeline-format-helper "error" errors))
         (warningsString (rtags-modeline-format-helper "warning" warnings))
         (errors-warnings
          (cond ((and errorsString warningsString) (concat errorsString "/" warningsString))
                (errorsString)
                (warningsString)
                (t nil))))
    (cond ((and progress errors-warnings) (format "RTags: %s %s " progress errors-warnings))
          (progress (format "RTags: %s " progress))
          (errors-warnings (format "RTags: %s " errors-warnings))
          (t ""))))

(defvar rtags-error-warning-count nil)
(make-variable-buffer-local 'rtags-error-warning-count)

(defun rtags-handle-check-style (filename data)
  ;; (message "parsing nodes %s" (rtags-buffer-file-name buffer))
  (let* ((line (nth 1 data))
         (column (nth 2 data))
         (length (nth 3 data))
         (severity (nth 4 data))
         (message (nth 5 data))
         (children (nth 6 data))
         (start)
         (end))
    (save-excursion
      (when (rtags-goto-line-col line column)
        (setq start (point))
        (setq end (cond (length (save-excursion
                                  (save-restriction
                                    (widen)
                                    (let (deactivate-mark)
                                      (rtags-goto-offset (+ (rtags-offset) length))
                                      (point)))))
                        ((let ((sym (thing-at-point 'symbol)))
                           (and sym (+ start (length sym)))))
                        (t (1+ start))))
        (when (looking-back "#" (point-at-bol))
          (decf start))
        (let ((overlay (make-overlay start (if (= start end)
                                               (min (1+ start) (point-max))
                                             end)
                                     (current-buffer))))
          (when children
            (overlay-put overlay 'rtags-error-children children))
          (overlay-put overlay 'rtags-error-message message)
          (overlay-put overlay 'rtags-error-severity severity)
          (overlay-put overlay 'rtags-error-start start)
          (overlay-put overlay 'rtags-error-end end)
          ;; (message "Got overlay %s:%d:%d %d - %d-%d - %s" filename line column (or length -1) start end severity)
          (overlay-put overlay 'face (cond ((eq severity 'error)
                                            (if rtags-error-warning-count
                                                (incf (car rtags-error-warning-count))
                                              (setq rtags-error-warning-count (cons 1 0)))
                                            'rtags-errline)
                                           ((eq severity 'warning)
                                            (if rtags-error-warning-count
                                                (incf (cdr rtags-error-warning-count))
                                              (setq rtags-error-warning-count (cons 0 1)))
                                            'rtags-warnline)
                                           ((eq severity 'fixit)
                                            (overlay-put overlay 'priority 1)
                                            'rtags-fixitline)
                                           ((eq severity 'skipped)
                                            'rtags-skippedline)
                                           (t
                                            'rtags-errline))))))
    (when start
      (let ((diagnostics-buffer (get-buffer rtags-diagnostics-buffer-name)))
        (when diagnostics-buffer
          (with-current-buffer diagnostics-buffer
            (setq buffer-read-only nil)
            (when (eq severity 'fixit)
              (insert (format "%s:%d:%d: fixit: %d-%d: %s\n" filename line column start end message)))
            (when (> (length message) 0)
              (insert (format "%s:%d:%d: %s: %s\n" filename line column severity message)))
            (setq buffer-read-only t)))))))

(defvar rtags-last-check-style nil)

(defun rtags-parse-check-style (checkstyle)
  (when checkstyle
    (setq rtags-last-check-style checkstyle))
  (dolist (cur checkstyle)
    (let* ((file (rtags-trampify (car cur)))
           (diags (cdr cur))
           (buf (find-buffer-visiting file)))
      (when buf
        (with-current-buffer buf
          (rtags-overlays-remove)
          (setq rtags-error-warning-count nil)
          (rtags-overlays-buffers-set buf diags)
          (dolist (diag diags)
            (rtags-handle-check-style file diag))
          ;; Manually trigger Flycheck to be in sync.
          (when (and (featurep 'flycheck-rtags)
                     (bound-and-true-p flycheck-mode))
            (flycheck-buffer)))))))

(defun rtags-get-buffer-create-no-undo (name)
  (or (get-buffer name)
      (let ((buf (get-buffer-create name)))
        (buffer-disable-undo buf)
        buf)))

(defvar rtags-diagnostics-errors nil
  "List of diagnostics errors.")

(defun rtags-parse-diagnostics (&optional buffer)
  (save-excursion
    (with-current-buffer (or buffer (rtags-get-buffer-create-no-undo rtags-diagnostics-raw-buffer-name))
      (while (and (goto-char (point-min))
                  (search-forward "\n" (point-max) t))
        (let* ((pos (1- (point)))
               (data (and (> (1- pos) (point-min))
                          (save-restriction
                            (narrow-to-region (point-min) pos)
                            (save-excursion
                              (goto-char (point-min))
                              (unless (looking-at "Can't seem to connect to server")
                                (condition-case nil
                                    (eval (read (current-buffer)))
                                  (error
                                   (message "****** Got Diagnostics Error ******")
                                   (setq rtags-diagnostics-errors
                                         (append rtags-diagnostics-errors
                                                 (list (buffer-substring-no-properties (point-min) (point-max)))))))))))))
          (cond ((not (listp data)))
                ((eq (car data) 'checkstyle)
                 (when rtags-spellcheck-enabled
                   (rtags-parse-check-style (cdr data))))
                ((eq (car data) 'progress)
                 (setq rtags-last-index (nth 2 data)
                       rtags-last-total (nth 3 data)))
                (t))
          (run-hooks 'rtags-diagnostics-hook)
          (forward-char 1)
          (delete-region (point-min) (point)))))))

(defun rtags-check-overlay (overlay)
  (when (and (overlayp overlay)
             (overlay-get overlay 'rtags-error-message)
             (not (active-minibuffer-window))
             (not cursor-in-echo-area))
    (rtags-display-overlay overlay (point))))

;;;###autoload
(defun rtags-is-running ()
  (interactive)
  (with-temp-buffer
    (rtags-call-rc :noerror t "--is-indexing")))

(defun rtags-format-context (str fraction)
  (when (string-match "^[ \t]+" str)
    (setq str (substring str (match-end 0))))
  (rtags-elide-text str (truncate (* (frame-width) fraction)) 'right))

(defun rtags-elide-text (str len part)
  (cond ((<= (length str) len) str)
        ((<= len 3) (substring str 0 len))
        ((eq part 'middle)
         (let ((part (- (/ len 2) 3)))
           (concat (substring str 0 part)
                   "..."
                   (substring str (- part)))))
        ((eq part 'left)
         (concat "..." (substring str (- (- len 3)))))
        ((eq part 'right)
         (concat (substring str 0 (- len 3)) "..."))
        (t (error "Wrong part"))))

(defun rtags-display-overlay (overlay point)
  (let* ((maxwidth (if rtags-display-current-error-as-tooltip
                       (window-width)
                     (frame-width)))
         (msg (rtags-elide-text (overlay-get overlay 'rtags-error-message) maxwidth 'middle))
         (bol (save-excursion
                (goto-char point)
                (point-at-bol)))
         (used (length msg))
         (children (and msg (overlay-get overlay 'rtags-error-children))))
    (when (> (length msg) 0)
      (when children
        (setq msg (concat msg "\n" (mapconcat #'(lambda (child)
                                                  (let* ((location (format "%s:%d:%d: "
                                                                           (file-name-nondirectory (or (car child) (rtags-buffer-file-name)))
                                                                           (nth 1 child)
                                                                           (nth 2 child)))
                                                         (ret (concat location
                                                                      (rtags-elide-text (nth 5 child) (- maxwidth (length location)) 'middle))))
                                                    (setq used (max used (length ret)))
                                                    ret))
                                              children
                                              "\n"))))
      (when rtags-display-current-error-as-tooltip
        ;;        (message "point %d bol %d (%d) used %d maxwidth %d" point bol (- point bol) used maxwidth)
        (while (>= (+ (- point bol) used) maxwidth)
          (decf point))
        (let ((popup-tip-max-width maxwidth))
          (popup-tip msg :point point :max-width maxwidth :around t))) ;; :face 'rtags-warnline)) ;;(overlay-get overlay 'face)))
      (when rtags-display-current-error-as-message
        (message "%s" msg)))))

(defvar rtags-update-current-error-timer nil)

(defun rtags-display-current-error ()
  (let ((current-overlays (overlays-at (point))))
    (setq rtags-update-current-error-timer nil)
    (while (and current-overlays (not (rtags-check-overlay (car current-overlays))))
      (setq current-overlays (cdr current-overlays)))))

(defun rtags-update-current-error ()
  (when rtags-update-current-error-timer
    (cancel-timer rtags-update-current-error-timer))
  (setq rtags-update-current-error-timer
        (and (or rtags-display-current-error-as-message
                 rtags-display-current-error-as-tooltip)
             (get-buffer rtags-diagnostics-buffer-name)
             (run-with-idle-timer
              rtags-error-timer-interval
              nil
              #'rtags-display-current-error))))

(defun rtags-is-rtags-overlay (overlay) (and overlay (overlay-get overlay 'rtags-error-message)))

(defun rtags-overlay-comparator (l r)
  (< (overlay-start l) (overlay-start r)))

(defun rtags-overlays-on-screen ()
  (sort (rtags-remove 'rtags-is-rtags-overlay (overlays-in (window-start) (window-end)) t) #'rtags-overlay-comparator))

(defvar rtags-highlighted-overlay nil)

;;;###autoload
(defun rtags-cycle-overlays-on-screen ()
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (let* ((overlays (rtags-overlays-on-screen))
           (idx (and rtags-highlighted-overlay (let ((i 0)
                                                     (overlay overlays))
                                                 (while (and overlay (not (eq (car overlay) rtags-highlighted-overlay)))
                                                   (setq overlay (cdr overlay))
                                                   (incf i))
                                                 (and overlay i))))
           (overlay (if (and idx (< (1+ idx) (length overlays)))
                        (nth (1+ idx) overlays)
                      (car overlays))))
      (when overlay
        (setq rtags-highlighted-overlay overlay)
        (rtags-display-overlay overlay (overlay-start overlay))))))

(defun rtags-fix-fixit-overlay (overlay)
  (let* ((msg (overlay-get overlay 'rtags-error-message))
         (severity (overlay-get overlay 'rtags-error-severity))
         (replacedata (and msg (cond ((string-match "^[^']*'\\([^']*\\)'.*did you mean '\\([^']*\\)'" msg)
                                      (cons (match-string-no-properties 1 msg)
                                            (match-string-no-properties 2 msg)))
                                     ((string-match "did you mean '\\(.*\\)'\\?$" msg)
                                      (cons nil (match-string-no-properties 1 msg)))
                                     (t nil))))
         (start (overlay-get overlay 'rtags-error-start))
         (end (overlay-get overlay 'rtags-error-end)))
    (when (and start
               end
               replacedata
               (eq severity 'fixit)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char start)
          (if (and (car replacedata)
                   (not (looking-at (car replacedata))))
              (message "RTags: Fixit doesn't seem valid, refusing to apply. Was expecting to replace '%s' but instead I see '%s'"
                       (car replacedata)
                       (buffer-substring-no-properties start end))
            (delete-char (- end start))
            (when (cdr replacedata)
              (insert (cdr replacedata)))
            (let ((overlays (overlays-in (point) (point-max)))
                  (change (- (length (cdr replacedata))
                             (- end start))))
              (while overlays
                (let ((overlay (car overlays)))
                  (when (eq (overlay-get overlay 'rtags-error-severity) 'fixit)
                    (let ((start (overlay-get overlay 'rtags-error-start))
                          (end (overlay-get overlay 'rtags-error-end)))
                      (overlay-put overlay 'rtags-error-start (+ start change))
                      (overlay-put overlay 'rtags-error-end (+ end change))))
                  (setq overlays (cdr overlays))))))))))))

;;;###autoload
(defun rtags-fix-fixit-at-point ()
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (let ((current-overlays (overlays-at (point))))
      (while (and current-overlays (not (rtags-fix-fixit-overlay (car current-overlays))))
        (setq current-overlays (cdr current-overlays))))))


(defvar rtags-container-timer nil)
(defvar rtags-container-last-location nil)
(defvar rtags-cached-current-container nil)
(defun rtags-update-current-container-cache ()
  (when (and (not (window-minibuffer-p (get-buffer-window)))
             (not (buffer-modified-p)))
    (if (and (or (eq major-mode 'c++-mode) (eq major-mode 'c-mode) (eq major-mode 'objc-mode))
             (file-exists-p (or (rtags-buffer-file-name) "")))
        (let ((loc (rtags-current-location)))
          (when (and loc (not (string= loc rtags-container-last-location)))
            (setq rtags-container-last-location loc)
            (let ((cur (rtags-current-container-name)))
              (when (not (string= cur rtags-cached-current-container))
                (setq rtags-cached-current-container cur)
                (run-hook-with-args 'rtags-current-container-hook rtags-cached-current-container)))))
      (when rtags-cached-current-container
        (setq rtags-cached-current-container nil)
        (run-hook-with-args 'rtags-current-container-hook rtags-cached-current-container)))))

(defun rtags-restart-find-container-timer ()
  (interactive)
  (when rtags-container-timer
    (cancel-timer rtags-container-timer))
  (setq rtags-container-timer
        (and rtags-track-container
             (funcall rtags-is-indexable (current-buffer))
             (run-with-idle-timer rtags-container-timer-interval nil #'rtags-update-current-container-cache))))

(defvar rtags-tracking-timer nil)
;;;###autoload
(defun rtags-restart-tracking-timer()
  (interactive)
  (when rtags-tracking-timer
    (cancel-timer rtags-tracking-timer))
  (setq rtags-tracking-timer
        (and rtags-tracking (string= (buffer-name) rtags-buffer-name)
             (run-with-idle-timer rtags-tracking-timer-interval nil
                                  (lambda ()
                                    (when (> (length (window-list)) 1)
                                      (rtags-show-in-other-window))
                                    (when rtags-tracking-timer
                                      (cancel-timer rtags-tracking-timer))
                                    (setq rtags-tracking-timer nil))))))

;;;###autoload
(defun rtags-post-command-hook ()
  (interactive)
  (when rtags-enabled
    (rtags-restart-update-current-project-timer)
    (rtags-update-current-error)
    (when rtags-close-taglist-on-focus-lost
      (rtags-close-taglist))
    (rtags-restart-find-container-timer)
    (rtags-restart-tracking-timer)
    (when (and rtags-highlight-current-line (rtags-is-rtags-buffer))
      (rtags-update-current-line))))

(defun rtags-after-save-hook ()
  (interactive)
  (when (and rtags-reindex-on-save (funcall rtags-is-indexable (current-buffer)))
    (rtags-call-rc :path (rtags-buffer-file-name)
                   :silent t
                   "-V" (rtags-buffer-file-name))))


(add-hook 'after-save-hook 'rtags-after-save-hook)
(add-hook 'post-command-hook 'rtags-post-command-hook)
;; (remove-hook 'post-command-hook 'rtags-post-command-hook)

(defun rtags-set-diagnostics-suspended-impl (suspended quiet)
  (setq rtags-diagnostics-suspended suspended)
  (if suspended
      (rtags-stop-diagnostics)
    (and rtags-autostart-diagnostics (rtags-diagnostics)))
  (unless quiet
    (message "RTags Diagnostics are %ssuspended" (if suspended "" "not "))))

;;;###autoload
(defun rtags-toggle-diagnostics-suspended (&optional quiet)
  (interactive)
  (rtags-set-diagnostics-suspended-impl (not rtags-diagnostics-suspended) quiet))

;;;###autoload
(defun rtags-set-diagnostics-suspended (&optional quiet)
  (interactive "P")
  (rtags-set-diagnostics-suspended-impl (y-or-n-p (format "Suspend RTags diagnostics%s? "
                                                          (or rtags-diagnostics-suspended " (currently suspended)" ""))) quiet))

;;;###autoload
(defun rtags-stop-diagnostics ()
  (interactive)
  (when (rtags-diagnostics-is-running)
    (kill-process rtags-diagnostics-process))

  (when (rtags-diagnostics-is-running)
    ;; kill above failed.
    (when (get-buffer rtags-diagnostics-buffer-name)
      (with-current-buffer rtags-diagnostics-buffer-name
        (when (and rtags-tramp-enabled (tramp-tramp-file-p default-directory))
          ;; diagnostics serves some remote host
          ;; We need to kill it within that context.
          (let ((result
                 (process-file "kill" nil nil nil
                               (int-to-string (process-id rtags-diagnostics-process)))))
            (unless (= result 0)
              ;; the kill above did not do. Let's send KILL
              (process-file "kill" nil nil nil "-9"
                            (int-to-string (process-id rtags-diagnostics-process)))))))))
  (when (get-buffer rtags-diagnostics-buffer-name)
    (kill-buffer rtags-diagnostics-buffer-name)))

(add-hook 'kill-emacs-hook 'rtags-stop-diagnostics) ;; remote diagnostics are not killed by default.

;;;###autoload
(defun rtags-clear-diagnostics ()
  (interactive)
  (let ((buf (get-buffer rtags-diagnostics-buffer-name)))
    (when (buffer-live-p buf)
      (let (deactivate-mark)
        (with-current-buffer buf
          (setq buffer-read-only nil)
          (goto-char (point-min))
          (delete-char (- (point-max) (point-min)))
          (setq buffer-read-only t))))
    (rtags-clear-all-diagnostics-overlays)))

(defun rtags-diagnostics-process-filter (process output)
  ;; Collect the diagnostics into rtags-diagnostics-raw-buffer-name until a newline is found
  ;; (with-current-buffer (rtags-get-buffer-create-no-undo "*RTags Debug*")
  ;;   (goto-char (point-max))
  ;;   (insert output))
  (with-current-buffer (rtags-get-buffer-create-no-undo rtags-diagnostics-raw-buffer-name)
    (goto-char (point-max))
    (insert output))
  ;; only try to process diagnostics if we detect an end condition
  (rtags-parse-diagnostics))

(defvar rtags-diagnostics-mode-map (make-sparse-keymap))
(define-key rtags-diagnostics-mode-map (kbd "q") 'rtags-call-bury-or-delete)
(define-key rtags-diagnostics-mode-map (kbd "c") 'rtags-clear-diagnostics)
(define-key rtags-diagnostics-mode-map (kbd "f") 'rtags-apply-fixit-at-point)
(set-keymap-parent rtags-diagnostics-mode-map compilation-mode-map)
(define-derived-mode rtags-diagnostics-mode compilation-mode "rtags-diagnostics"
  (when (rtags-buffer-file-name)
    (error "Set buffer with file %s read only " (rtags-buffer-file-name)))
  (setq buffer-read-only t))

(defun rtags-diagnostics-sentinel (process event)
  (let ((status (process-status process)))
    (when (memq status '(exit signal closed failed))
      (setq rtags-last-update-current-project-buffer nil)
      (rtags-clear-diagnostics))))

;;;###autoload
(defun rtags-diagnostics (&optional restart nodirty)
  (interactive "P")
  (when rtags-enabled
    (let ((rc (rtags-executable-find "rc")))
      (when rc
        (when restart
          (rtags-stop-diagnostics))
        (let ((buf (rtags-get-buffer-create-no-undo rtags-diagnostics-buffer-name)))
          (when (and (not (rtags-has-diagnostics))
                     (not rtags-diagnostics-starting))
            (let ((rtags-diagnostics-starting t))
              (with-current-buffer buf
                (rtags-diagnostics-mode))
              (unless nodirty
                (rtags-reparse-file))
              (let ((process-connection-type (not rtags-diagnostics-use-pipe))) ;; use a pipe if rtags-diagnostics-use-pipe is t
                (let ((rawbuf (get-buffer rtags-diagnostics-raw-buffer-name)))
                  (when rawbuf
                    (kill-buffer rawbuf)))
                (let ((rc-args '("-m" "--elisp")))
                  (when (> (length rtags-socket-file) 0)
                    (push (rtags--get-socket-file-switch) rc-args))
                  (unless rtags-spellcheck-enabled
                    (push "--no-spell-checking" rc-args))
                  (setq rtags-diagnostics-process
                        (apply #'start-file-process "RTags Diagnostics" buf rc rc-args)))
                (set-process-filter rtags-diagnostics-process #'rtags-diagnostics-process-filter)
                (set-process-sentinel rtags-diagnostics-process 'rtags-diagnostics-sentinel)
                (set-process-query-on-exit-flag rtags-diagnostics-process nil)
                (rtags-clear-diagnostics)
                (rtags-schedule-buffer-list-update)))))
        (when (and (called-interactively-p 'any) (rtags-is-running))
          (switch-to-buffer-other-window rtags-diagnostics-buffer-name)
          (other-window 1))))))

(defvar rtags-indexed nil)
(defvar rtags-file-managed nil)

(defun rtags-buffer-status (&optional buffer)
  (when rtags-enabled
    (let* ((fn (rtags-buffer-file-name buffer))
           (path (cond (fn (and (file-exists-p fn) fn))
                       (dired-directory)
                       (default-directory)
                       (t nil))))
      (when path
        (setq path (expand-file-name path))
        (with-temp-buffer
          (rtags-call-rc :noerror t :silent-query t :path path "-T" path)
          (goto-char (point-min))
          (cond ((looking-at "indexed") 'rtags-indexed)
                ((looking-at "managed") 'rtags-file-managed)
                (t nil)))))))

;;;###autoload
(defun rtags-compilation-flags ()
  (interactive)
  (let ((path (rtags-buffer-file-name)))
    (when path
      (with-temp-buffer
        (rtags-call-rc :path path "--source" path "--compilation-flags-only" "--compilation-flags-split-line")
        (let ((str (buffer-substring-no-properties (point-min) (point-max))))
          (when (rtags-called-interactively-p)
            (message "%s" (combine-and-quote-strings (split-string str "\n") " ")))
          (split-string str "\n" t))))))

(defun rtags-is-working (&optional buffer)
  (let ((path (expand-file-name (or (rtags-buffer-file-name buffer) dired-directory default-directory))))
    (with-temp-buffer
      ;;(message ":debug: rtags-is-working: buffer=%s, path=%s" buffer path)
      (rtags-call-rc :output (list t t) :silent-query t :path path "-s" "jobs")
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        ;;(message ":debug: text=%s" text)
        (cond ((string-match "Dirty" text) t)
              ((string-match "jobs" text) nil) ; 'jobs' without 'dirty' = not working
              (t t))))))


(defun rtags-is-indexed (&optional buffer)
  (let ((path (rtags-buffer-file-name buffer)))
    (cond ((not path) nil)
          ((equal (rtags-buffer-status buffer) 'rtags-indexed)))))

(defun rtags-has-filemanager (&optional buffer)
  (rtags-buffer-status buffer))

(defun rtags-delete-rtags-windows ()
  (let* ((windows (window-list))
         (count (length windows)))
    (while windows
      (when (rtags-is-rtags-buffer (window-buffer (car windows)))
        (if (= count 1)
            (bury-buffer (window-buffer (car windows)))
          (decf count)
          (delete-window (car windows))))
      (setq windows (cdr windows)))))

(defun rtags-format-results ()
  "Create a bookmark for each match and format the buffer."
  (let ((startpos))
    (goto-char (point-max))
    (when (= (point-at-bol) (point-max))
      (delete-char -1))
    (goto-char (point-min))
    (when (looking-at "Functions called from:")
      (forward-line 1)
      (setq startpos (point)))
    (while (not (eobp))
      (when (looking-at "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\):?[ \t]*\\(.*\\)$")
        ;; (message "matched at %d:%d" (point) rtags-buffer-bookmarks)
        (let* ((start (point-at-bol))
               (end (min (point-max) (1+ (point-at-eol))))
               (buffer (and rtags-use-bookmarks (get-file-buffer (rtags-absolutify (match-string-no-properties 1)))))
               (line (and buffer (string-to-number (match-string-no-properties 2))))
               (bookmark-idx)
               (column (and buffer (string-to-number (match-string-no-properties 3)))))
          (when buffer
            (let (deactivate-mark)
              (with-current-buffer buffer
                (save-excursion
                  (save-restriction
                    (widen)
                    (when (and (rtags-goto-line-col line column)
                               (rtags-bookmark-set (format "RTags_%d" rtags-buffer-bookmarks)))
                      (setq bookmark-idx rtags-buffer-bookmarks)
                      (incf rtags-buffer-bookmarks)))))))
          (when rtags-verbose-results
            (goto-char (match-end 4))
            (insert "\n" rtags-verbose-results-delimiter)
            (goto-char (match-beginning 4))
            (insert "\n    ")
            (incf end 5))
          (set-text-properties start end (list 'rtags-bookmark-index (cons bookmark-idx start)))))
      (forward-line 1))
    (rtags-mode)
    (when startpos
      (goto-char startpos))))

(defun rtags-handle-results-buffer (&optional noautojump quiet path other-window)
  "Handle results from RTags. Should be called with the results buffer
as current.

The option OTHER-WINDOW is only applicable if RTags is configured not to
show the results immediately. If non-nil, show the first match in the
other window instead of the current one."
  (rtags-reset-bookmarks)
  (set-text-properties (point-min) (point-max) nil)
  (when path
    (setq rtags-current-file path))

  (cond ((= (point-min) (point-max))
         (unless quiet
           (message "RTags: No results"))
         nil)
        ((or rtags-last-request-not-indexed rtags-last-request-not-connected) nil)
        ((= (count-lines (point-min) (point-max)) 1)
         (let ((string (buffer-string)))
           (push-mark nil t)
           (rtags-goto-location string nil other-window)
           t))
        (t
         (rtags-format-results)
         (unless quiet
           (message "RTags: Found %d locations."
                    (count-lines (point-min) (point-max))))
         ;; Optionally jump to first result and open results buffer
         (when (and rtags-popup-results-buffer
                    (eq rtags-display-result-backend 'default)
                    (rtags-switch-to-buffer rtags-buffer-name t))
           (shrink-window-if-larger-than-buffer))
         (cond ((eq rtags-display-result-backend 'default)
                (when (and rtags-jump-to-first-match (not noautojump))
                  (if rtags-popup-results-buffer
                      (rtags-select-other-window)
                    (rtags-select other-window))))
               ((eq rtags-display-result-backend 'helm)
                (require 'helm-rtags)
                (helm :sources '(helm-rtags-source)))
               ((eq rtags-display-result-backend 'ivy)
                (require 'ivy-rtags)
                (ivy-rtags-read)))
         t)))

(defun rtags-filename-complete (string predicate code)
  (let ((complete-list (make-vector 63 0)))
    (when (or (string-match "\\(.*\\),[0-9]+" string)
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
            (when (looking-at match-string-no-properties)
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
(define-derived-mode rtags-taglist-mode rtags-mode "rtags-taglist")

;;;###autoload
(defun rtags-close-taglist ()
  (interactive)
  (unless rtags-taglist-protected
    (let ((buf (get-buffer rtags-buffer-name)))
      (when (and buf
                 (not (eq (current-buffer) buf))
                 (eq (with-current-buffer buf major-mode) 'rtags-taglist-mode))
        (let ((windows (window-list)))
          (while windows
            (when (eq (window-buffer (car windows)) buf)
              (delete-window (car windows))
              (setq windows nil))
            (setq windows (cdr windows))))))))

;; category (list (text . (location . linenumber)))
(defun rtags-taglist-insert-category (category name)
  (let ((max 0))
    (when category
      (insert "\n")
      (set-mark-command nil)
      (let ((start (point)) end)
        (insert name ":")
        (setq end (point))
        (facemenu-set-face "header-line" start end))
      (insert "\n\n")
      (while category
        (add-to-list 'rtags-taglist-locations (cons (line-number-at-pos) (cdar category)))
        (let* ((text (caar category))
               (len (length text)))
          (insert " " text "\n")
          (setq max (max len max)))
        (setq category (cdr category))))
    max))

;;;###autoload
(defun rtags-taglist (&optional dest-window)
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (unless (rtags-buffer-file-name)
      (error "rtags-taglist must be run from a buffer visiting a file"))
    (rtags-delete-rtags-windows)
    (rtags-location-stack-push)
    (setq rtags-taglist-locations nil)
    (let* ((fn (rtags-buffer-file-name)) functions classes variables enums macros other)
      (with-temp-buffer
        (rtags-call-rc :path fn :path-filter fn "-F" "--cursor-kind" "--display-name" "--no-context")
        ;; (message (buffer-string))
        (unless (= (point-min) (point-max))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
              (when (string-match "^\\(.*:\\)\\([0-9]+\\)\\(:[0-9]+:\\)\t\\(.*\\)\t\\(.*\\)$" line)
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
            (forward-line))))
      (when (or functions classes variables enums macros other)
        (when (not dest-window)
          (delete-other-windows))
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
          (when (not dest-window)
            (split-window-horizontally (min (/ (frame-width) 2) (+ 2 max))))
          (rtags-switch-to-buffer buf)
          (rtags-taglist-mode)
          (deactivate-mark))))))

(defun rtags-is-class-hierarchy-buffer ()
  (when (eq major-mode 'rtags-mode)
    (save-excursion
      (goto-char (point-min))
      (looking-at "\\(Subclasses:\\|Superclasses:\\)$"))))

;;;###autoload
(defun rtags-select (&optional other-window remove show)
  (interactive "P")
  (push-mark nil t)
  (let* ((idx (get-text-property (point) 'rtags-bookmark-index))
         (line (line-number-at-pos))
         (bookmark (and (car idx) (format "RTags_%d" (car idx))))
         (window (selected-window)))
    (cond ((eq major-mode 'rtags-taglist-mode)
           (rtags-goto-location (cdr (assoc line rtags-taglist-locations)) nil other-window)
           (when rtags-close-taglist-on-selection
             (rtags-close-taglist)))
          ((rtags-is-class-hierarchy-buffer)
           (save-excursion
             (goto-char (point-at-bol))
             (let ((loc (and (looking-at "^[^\t]*\t\\(.*:[0-9]+:[0-9]+:\\)\t") (match-string 1))))
               (when loc
                 (rtags-goto-location loc nil other-window)))))
          ((string= (buffer-name) "*RTags Dependencies*")
           (let ((cur (rtags-dependency-tree-current-file)))
             (when cur
               (rtags-goto-location (car cur) nil other-window))))
          ((string= (buffer-name) "*RTags Location Stack*")
           (let ((index (- (length rtags-location-stack) line)))
             (setq rtags-location-stack-index index)
             (rtags-goto-location (nth rtags-location-stack-index rtags-location-stack) t other-window t)
             (rtags-location-stack-visualize-update)))
          ((and (car idx)
                (>= rtags-buffer-bookmarks (car idx))
                (member bookmark (rtags-bookmark-all-names)))
           (when other-window
             (when (= (length (window-list)) 1)
               (funcall rtags-split-window-function))
             (other-window 1))
           (let ((switch-to-buffer-preserve-window-point nil)) ;; this can mess up bookmarks
             (bookmark-jump bookmark))
           (rtags-location-stack-push))
          (t
           (when (cdr idx)
             (goto-char (cdr idx)))
           (rtags-goto-location (buffer-substring-no-properties (save-excursion
                                                                  (goto-char (point-at-bol))
                                                                  (skip-chars-forward " ")
                                                                  (point))
                                                                (point-at-eol)) nil other-window)
           (when bookmark
             (bookmark-set bookmark))))
    (if remove
        (delete-window window)
      (when show
        (select-window window)))))

;;;###autoload
(defun rtags-select-other-window (&optional not-other-window)
  (interactive "P")
  (rtags-select (not not-other-window)))

;;;###autoload
(defun rtags-select-caller (&optional not-other-window)
  (interactive "P")
  (let ((file (save-excursion
                (goto-char (point-min))
                (and (looking-at "Functions called from: \\(.*?\\):[0-9]+:[0-9]+:")
                     (match-string 1)))))
  (when file
    (save-excursion
      (goto-char (point-at-bol))
      (when (looking-at ".*called from - .*?:\\([0-9]+\\):\\([0-9]+\\):")
        (rtags-goto-location (concat file ":" (match-string 1) ":" (match-string 2)) (not not-other-window)))))))


;;;###autoload
(defun rtags-select-caller-other-window ()
  (interactive)
  (rtags-select-caller t))

;;;###autoload
(defun rtags-show-in-other-window ()
  (interactive)
  ;; (message "About to show")
  (rtags-select t nil t))

;;;###autoload
(defun rtags-select-and-remove-rtags-buffer ()
  (interactive)
  (rtags-select t t))

;;;###autoload
(defun rtags-imenu ()
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (rtags-delete-rtags-windows)
    (rtags-location-stack-push)
    (let* ((fn (rtags-buffer-file-name))
           (alternatives (with-temp-buffer
                           (rtags-call-rc :path fn :path-filter fn
                                          "--kind-filter" rtags-imenu-kind-filter
                                          "--elisp"
                                          (when rtags-wildcard-symbol-names "--wildcard-symbol-names")
                                          "--list-symbols")
                           (eval (read (buffer-string)))))
           (match (and (> (length alternatives) 1)
                       (completing-read "Symbol: " alternatives nil t))))
      (when match
        (rtags-goto-location (with-temp-buffer (rtags-call-rc :path-filter fn :path fn "-F" match "--no-context" "--absolute-path") (buffer-string)))
        (message "RTags: No symbols")))))

(defun rtags-append (txt)
  (goto-char (point-min))
  (while (< (point-at-eol) (point-max))
    (goto-char (point-at-eol))
    (insert txt)
    (forward-line)))

;;;###autoload
(defun rtags-copy-and-print-current-location()
  (interactive)
  (let ((loc (rtags-current-location)))
    (if (not loc)
        (message "No current location!")
      (kill-new loc)
      (message loc))))

(defun rtags-all-files (prefer-exact)
  (with-temp-buffer
    (rtags-call-rc "-P" "--elisp" (when rtags-find-file-case-insensitive "-I") (when prefer-exact "-A"))
    (and (> (point-max) (point-min))
         (eval (read (current-buffer))))))

(defvar rtags-find-file-history nil)
;;;###autoload
(defun rtags-find-file (&optional prefix default-tag)
  (interactive "P")
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (rtags-delete-rtags-windows)
    (rtags-location-stack-push)
    (let ((tagname (or default-tag (rtags-current-symbol t)))
          (prompt)
          (input)
          (offset)
          (line)
          (column)
          (prefer-exact rtags-find-file-prefer-exact-match))
      (when prefix
        (setq prefer-exact (not prefer-exact)))
      (if (> (length tagname) 0)
          (setq prompt (format "%s (default: %s): " rtags-find-file-prompt tagname))
        (setq prompt (format "%s: " rtags-find-file-prompt)))
      (rtags-is-indexed)
      (setq input
            (if rtags-use-filename-completion
                (if (fboundp 'completing-read-default)
                    (completing-read-default prompt #'rtags-filename-complete nil nil nil 'rtags-find-file-history)
                  (completing-read prompt #'rtags-filename-complete nil nil nil 'rtags-find-file-history))
              (completing-read prompt (rtags-all-files prefer-exact) nil nil nil 'rtags-find-file-history)))
      (setq rtags-find-file-history (rtags-remove-last-if-duplicated rtags-find-file-history))
      (cond ((null input) nil)
            ((string-match "\\(.*\\),\\([0-9]+\\)" input)
             (setq tagname (match-string-no-properties 1 input))
             (setq offset (string-to-number (match-string-no-properties 2 input))))
            ((string-match "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)" input)
             (setq tagname (match-string-no-properties 1 input))
             (setq line (string-to-number (match-string-no-properties 2 input)))
             (setq column (string-to-number (match-string-no-properties 3 input))))
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
        (rtags-call-rc "-P" tagname
                       (when rtags-find-file-absolute "-K")
                       (when rtags-find-file-case-insensitive "-I")
                       (when prefer-exact "-A"))
        (and (= (point-min) (point-max))
             (string-match "[^/]\\.\\.[^/]" tagname)
             (rtags-call-rc "-P"
                            (replace-regexp-in-string "\\([^/]\\)\\.\\.\\([^/]\\)" "\\1.\\2" tagname)
                            (when rtags-find-file-absolute "-K")
                            (when rtags-find-file-case-insensitive "-I")
                            (when prefer-exact "-A")))

        (cond (offset (rtags-append (format ",%d" offset)))
              ((and line column) (rtags-append (format ":%d:%d" line column)))
              ((and line) (rtags-append (format ":%d" line)))
              (t nil))
        ;; (message (format "Got lines and shit %d\n[%s]" (count-lines (point-min) (point-max)) (buffer-string)))
        (goto-char (point-min))
        (cond ((= (point-min) (point-max)) t)
              ((= (count-lines (point-min) (point-max)) 1) (rtags-goto-location (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
              (t (rtags-switch-to-buffer rtags-buffer-name t)
                 (shrink-window-if-larger-than-buffer)
                 (rtags-mode)))))))

;;;###autoload
(defun rtags-show-rtags-buffer ()
  (interactive)
  (when (get-buffer rtags-buffer-name)
    (display-buffer rtags-buffer-name)))

;;;###autoload
(defun rtags-fixit (&optional ediff buffer)
  (interactive "P")
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (save-some-buffers)
    (unless buffer
      (setq buffer (current-buffer)))
    (save-excursion
      (let* ((path (rtags-buffer-file-name buffer))
             (tempbuf nil)
             (buffertext (when ediff (with-current-buffer buffer (buffer-string))))
             (min (line-number-at-pos
                   (if mark-active
                       (region-beginning)
                     (point-min))))
             (max (line-number-at-pos
                   (if mark-active
                       (region-end)
                     (point-max))))
             (line nil))
        (with-temp-buffer
          (rtags-call-rc :path path "--fixits" path)
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
              (when (string-match "^\\([0-9]+\\):\\([0-9]+\\) \\([0-9]+\\) \\(.*\\)$" line)
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
                      (when (rtags-goto-line-col line col)
                        (delete-char length) ;; may be 0
                        (insert text)))))))
            ;; (message (format "got something %d to %d => [%s]" start end text))))
            (forward-line)))
        (when tempbuf
          (let ((tempbufname (format "/tmp/rtags-fixit-%s" (file-name-nondirectory path))))
            (with-current-buffer tempbuf (write-file tempbufname))
            (kill-buffer tempbuf)
            (ediff path tempbufname)))))))

(defun rtags-current-symbol-name (&optional location)
  (let* ((symbolname (cdr (assoc 'symbolName (rtags-symbol-info-internal :location location))))
         (visual (and symbolname
                      (with-temp-buffer
                        (insert symbolname)
                        (goto-char (point-min))
                        (when (re-search-forward "(" nil t)
                          (delete-region (1- (point)) (point-max)))
                        (goto-char (point-max))
                        (when (re-search-backward "[: &*]" nil t)
                          (delete-region (point-min) (1+ (point)))
                          (buffer-string)))))
         (token (rtags-current-token)))
    (or (and visual (string= visual token) symbolname)
        token)))

(defconst rtags-backward-token-symbolchars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_~")
(defun rtags-backward-token ()
  (or (< (skip-chars-backward " \t\n") 0)
      (< (skip-chars-backward "[]") 0) ;; for lambdas
      (< (skip-chars-backward "A[A-Za-z_]") 0)
      (< (skip-chars-backward rtags-backward-token-symbolchars) 0)
      (backward-char)))

(defun rtags-forward-token ()
  (or (< (skip-chars-forward " \t\n") 0)
      (< (skip-chars-forward "[]") 0) ;; for lambdas
      (< (skip-chars-forward "A[A-Za-z_]") 0)
      (< (skip-chars-forward rtags-backward-token-symbolchars) 0)
      (forward-char)))

(defun rtags-current-container ()
  (save-excursion
    (save-restriction
      (widen)
      (let ((done)
            (line (line-number-at-pos))
            (col (1+ (- (point) (point-at-bol))))
            (max 16)
            (container))
        (while (and (> (point) (point-min))
                    (> max 0)
                    (not done))
          (decf max)
          (let ((token (rtags-current-token)))
            (when (cond ((null token))
                        ((member token rtags-c++-keywords))
                        ((member token rtags-c++-types))
                        (t
                         (let ((info (rtags-symbol-info-internal :silent t :parents t)))
                           (cond (rtags-last-request-not-indexed (setq done t))
                                 (rtags-last-request-not-connected (setq done t))
                                 ((setq container (or (cdr (assoc 'parent info))
                                                      (and (assoc 'container info) info))))
                                 (info (setq done t))
                                 (t)))
                         (when container
                           (setq done t)
                           (let ((start-line (cdr (assoc 'startLine container)))
                                 (end-line (cdr (assoc 'endLine container)))
                                 (start-column (cdr (assoc 'startColumn container)))
                                 (end-column (cdr (assoc 'endColumn container))))
                             (when (or (< line start-line)
                                       (and (= line start-line)
                                            (< col start-column))
                                       (> line end-line)
                                       (and (= line end-line)
                                            (>= col end-column)))
                               (setq container nil))))
                         (not done)))
              (rtags-backward-token))))
        container))))

(defun rtags-current-container-name ()
  (cdr (assoc 'symbolName (rtags-current-container))))

(defun rtags-cursor-extent (&optional location)
  (let ((symbol-info (rtags-symbol-info :location location)))
    (when (string-match "^Range: \\([0-9]+\\)-\\([0-9]+\\)$" symbol-info)
      (let ((start (+ (string-to-number (match-string-no-properties 2 symbol-info)) 1))
            (end (+ (string-to-number (match-string-no-properties 3 symbol-info)) 1)))
        (cons start end)))))

(defvar rtags-other-window-window nil)
;;;###autoload
(defun rtags-remove-other-window ()
  (interactive)
  (let ((ret ""))
    (when (and (> (length (window-list nil nil)) 1)
               (windowp rtags-other-window-window)
               (window-live-p rtags-other-window-window))
      (select-window rtags-other-window-window)
      (setq ret (rtags-current-location))
      (delete-window rtags-other-window-window)
      (setq rtags-other-window-window nil))
    ret))

(defvar rtags-last-update-current-project-buffer nil)
;;;###autoload
(defun rtags-update-current-project ()
  (interactive)
  (when (and (or (rtags-buffer-file-name) dired-directory)
             (not (eq (current-buffer) rtags-last-update-current-project-buffer))
             default-directory
             (not (and (tramp-tramp-file-p default-directory) (not rtags-tramp-enabled)))
             (file-directory-p default-directory))
    (setq rtags-last-update-current-project-buffer (current-buffer))
    (let* ((rc (rtags-executable-find "rc"))
           (path (rtags-untrampify (or (rtags-buffer-file-name) default-directory)))
           (arguments (list "-T" path "--diagnose" path "--silent-query")))
      (when (and rtags-completions-enabled
                 (or (and (boundp 'company-mode) company-mode)
                     (and (boundp 'auto-complete-mode) auto-complete-mode)))
        (push "-b" arguments))
      (when rc
        (apply #'start-file-process "rtags-update-current-project" nil rc arguments))))
  t)

(defvar rtags-update-current-project-timer nil)
(defun rtags-restart-update-current-project-timer ()
  (interactive)
  (when rtags-update-current-project-timer
    (cancel-timer rtags-update-current-project-timer))
  (setq rtags-update-current-project-timer
        (run-with-idle-timer rtags-update-current-project-timer-interval nil #'rtags-update-current-project)))

;;;###autoload
(defun rtags-show-target-in-other-window (&optional dest-window center-window
                                                    try-declaration-first)
  "DEST-WINDOW : destination window. Can be nil; in this case the current window is split
according to `rtags-other-window-window-size-percentage'.
CENTER-WINDOW : if true the target window is centered.
TRY-DECLARATION-FIRST : first try to find the declaration of the item, then the
definition."
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (let ((target (if try-declaration-first
                      (rtags-target-declaration-first)
                    (rtags-target))))
      (unless target
        (let ((token (rtags-current-token)))
          (when token
            (with-temp-buffer
              (rtags-call-rc "-G" "-N" "-F" token)
              (when (= (count-lines (point-min) (point-max)) 1)
                (setq target (buffer-substring-no-properties (point) (- (point-max) 1))))))))
      (when target
        (let ((win (selected-window)))
          (if dest-window
              (setq rtags-other-window-window dest-window)
            (let ((other-window-content (rtags-remove-other-window))
                  (height (* (window-height) (- 100 rtags-other-window-window-size-percentage))))
              (unless (string= target other-window-content)
                (setq height (/ height 100))
                (setq rtags-other-window-window (funcall rtags-split-window-function nil height)))))
          (select-window rtags-other-window-window)
          (rtags-goto-location target)
          (recenter-top-bottom (when (not center-window) 0))
          (select-window win))))))

(defun rtags-find-symbols-by-name-internal (prompt switch &optional filter regexp-filter other-window)
  (rtags-delete-rtags-windows)
  (rtags-location-stack-push)
  (let ((tagname (rtags-current-symbol))
        (path (rtags-buffer-file-name))
        input)
    (if (> (length tagname) 0)
        (setq prompt (concat prompt ": (default: " tagname ") "))
      (setq prompt (concat prompt ": ")))
    (setq input (cond ((fboundp 'completing-read-default)
                       (completing-read-default prompt #'rtags-symbolname-complete nil nil nil 'rtags-symbol-history))
                      (t (completing-read prompt #'rtags-symbolname-complete nil nil nil 'rtags-symbol-history))))
    (setq rtags-symbol-history (rtags-remove-last-if-duplicated rtags-symbol-history))
    (when (not (equal "" input))
      (setq tagname input))
    (with-current-buffer (rtags-get-buffer)
      (rtags-call-rc :path path switch tagname :path-filter filter
                     :path-filter-regex regexp-filter
                     (when rtags-wildcard-symbol-names "--wildcard-symbol-names")
                     (when rtags-symbolnames-case-insensitive "-I")
                     (unless rtags-print-filenames-relative "-K"))
      (rtags-handle-results-buffer nil nil path other-window))))

(defun rtags-symbolname-completion-get (string)
  (with-temp-buffer
    (rtags-call-rc "--elisp" "-S" string
                   (when rtags-symbolnames-case-insensitive "-I")
                   (when rtags-wildcard-symbol-names "--wildcard-symbol-names"))
    ;; (when rtags-rc-log-enabled
    ;;   (rtags-log (buffer-string)))
    (eval (read (buffer-string)))))

(defun rtags-symbolname-completion-exactmatch (string)
  (with-temp-buffer
    (rtags-call-rc "-N" "-F" string
                   (when rtags-symbolnames-case-insensitive "-I")
                   (when rtags-wildcard-symbol-names "--wildcard-symbol-names"))
    (> (point-max) (point-min))))

(defun rtags-symbolname-complete (string predicate code)
  ;; (message "CALLED %s %s %s"
  ;;          string predicate
  ;;          (cond ((eq code nil) "nil")
  ;;                ((eq code t) "t")
  ;;                ((eq code 'lambda) "lambda")))

  (cond ((null code)
         (let* ((alternatives (rtags-symbolname-completion-get string))
                (attempt (try-completion string alternatives predicate)))
           ;; (message "%s %d %d %s %s" string (length alternatives)
           ;;          (if rtags-wildcard-symbol-names 1 0)
           ;;          attempt
           ;;          (and (string-match '\\*' string) "yes"))

           ;; (if (and rtags-wildcard-symbol-names
           ;;          (not attempt)
           ;;          (> (length alternatives) 0)
           ;;          (string-match "\\*" string))
           ;;     (progn
           ;;       (message "RETURNING STRING")
           ;;       string)
           ;;   attempt)))
           attempt))
        ((eq code t)
         (rtags-symbolname-completion-get string))
        ((eq code 'lambda)
         (rtags-symbolname-completion-exactmatch string))
        (t nil)))

(defun rtags-offset-for-line-column (line col)
  (let (deactivate-mark)
    (save-excursion
      (save-restriction
        (widen)
        (and (rtags-goto-line-col line col) (rtags-offset))))))

(defun rtags-range-visible (start end)
  (and (>= start (window-start))
       (<= start (window-end))
       (<= end (window-end))))

;;;###autoload
(defun rtags-suspend-file(&optional arg)
  (interactive)
  (let ((buffer (rtags-buffer-file-name)))
    (when buffer
      (with-temp-buffer
        (rtags-call-rc :path buffer "-X" buffer (or arg "on"))
        (if (> (point-max) (point-min))
            (message (buffer-substring-no-properties (point-min) (1- (point-max))))
          (message (buffer-string)))))))

;;;###autoload
(defun rtags-unsuspend-file()
  (interactive)
  (rtags-suspend-file "off"))

;;;###autoload
(defun rtags-toggle-file-suspended()
  (interactive)
  (rtags-suspend-file "toggle"))

;;;###autoload
(defun rtags-clear-suspended-files()
  (interactive)
  (let ((buffer (rtags-buffer-file-name)))
    (when buffer
      (with-temp-buffer
        (rtags-call-rc :path buffer "-X" "clear")
        (if (> (point-max) (point-min))
            (message (buffer-substring-no-properties (point-min) (1- (point-max))))
          (message (buffer-string)))))))

;;;###autoload
(defun rtags-suspend-all-files()
  (interactive)
  (let ((buffer (rtags-buffer-file-name)))
    (when buffer
      (with-temp-buffer
        (rtags-call-rc :path buffer "-X" "all")
        (if (> (point-max) (point-min))
            (message (buffer-substring-no-properties (point-min) (1- (point-max))))
          (message (buffer-string)))))))

;;;###autoload
(defun rtags-list-suspended-files()
  (interactive)
  (let ((buffer (rtags-buffer-file-name)))
    (when buffer
      (with-temp-buffer
        (rtags-call-rc :path buffer "-X")
        (if (> (point-max) (point-min))
            (message (buffer-substring-no-properties (point-min) (1- (point-max))))
          (message (buffer-string)))))))

(defvar rtags-last-compiled-source nil)
;;;###autoload
(defun rtags-compile-file (&optional buffer)
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (let ((source (cond ((stringp buffer) buffer)
                        ((bufferp buffer) (rtags-buffer-file-name buffer))
                        (t (rtags-buffer-file-name)))))
      (with-temp-buffer
        (setq rtags-last-compiled-source source)
        (rtags-call-rc :path source "--sources" source "--compilation-flags-only" "--compilation-flags-split-line" "--compilation-flags-pwd")
        (let* ((commands (mapcar (lambda (build)
                                   (let ((lines (split-string build "\n" t)))
                                     (cons (combine-and-quote-strings (cdr lines))
                                           (substring (car lines) 5))))
                                 (split-string (buffer-string) "(\n)?pwd: " t)))
               (old-compile-command compile-command)
               (command (car commands)))
          (when (cond ((> (length commands) 1)
                       (let ((answer (completing-read "Choose build: " commands)))
                         (when answer
                           (setq command (assoc answer commands)))))
                      ((null commands) (message "RTags doesn't know how to compile this file") nil)
                      (t))
            (cd (cdr command))
            (compile (car command))
            (setq compile-command old-compile-command)))))))

;;;###autoload
(defun rtags-recompile-file ()
  (interactive)
  (if rtags-last-compiled-source
      (rtags-compile-file rtags-last-compiled-source)
    (message "No file to recompile")))

(defun rtags-dummy-includes-func()
  "Dummy function, returns `rtags-rdm-includes'."
  rtags-rdm-includes)

(defvar rtags-rdm-process nil)

;;;###autoload
(defun rtags-quit-rdm ()
  "Quit the RTags process (rdm)."
  (interactive)
  (let ((rc (rtags-executable-find "rc")))
    (when rc
      (process-file rc nil nil nil "--quit-rdm"))))

(defun rtags-rdm-includes ()
  (mapconcat 'identity
             (mapcar
              (lambda (item) (concat "-I" item))
              (funcall rtags-includes-func)) " "))

(defun rtags-command ()
  "Shell command used to start the `rtags-server' process."
  (format "%s %s %s"
          (rtags-executable-find "rdm")
          (rtags-rdm-includes)
          rtags-process-flags))

(defun rtags-cancel-process ()
  "Stop the RTags process."
  (if (not rtags-rdm-process)
      (message "No RTags process running (rdm)...")
    (delete-process rtags-rdm-process)
    (kill-buffer "*rdm*")))

;;;###autoload
(defun rtags-restart-process ()
  "Restart the RTags process (rdm)."
  (interactive)
  (rtags-cancel-process)
  (rtags-start-process-unless-running))

;;;###autoload
(defun rtags-start-process-unless-running ()
  "Launch the RTags process (rdm) if it's not already started."
  (interactive)
  (let ((rtags-server-executable (rtags-executable-find "rdm")))
    (cond
     ;; Already started, nothing need to be done
     ((or (and (processp rtags-rdm-process)
               (not (eq (process-status rtags-rdm-process) 'exit))
               (not (eq (process-status rtags-rdm-process) 'signal)))
          (dolist (pid (reverse (list-system-processes))) ;; Check in the sys-processes for rdm
            (let* ((attrs (process-attributes pid))
                   (pname (cdr (assoc 'comm attrs)))
                   (uid (cdr (assoc 'euid attrs))))
              (when (and (eq uid (user-uid))
                         (or (string-equal pname "rdm")
                             (string-equal pname "rdm.exe")))
                (return t))))))

     ;; Executable not found or invalid
     ((or (null rtags-server-executable)
          (null (file-executable-p rtags-server-executable))
          (file-directory-p rtags-server-executable))
      (error "Can't start the process `%s'. Please check the value of the variable `rtags-path'."
             rtags-server-executable))
     (t
      (let ((process-connection-type (not rtags-rdm-process-use-pipe)))
        (setq rtags-rdm-process (start-file-process-shell-command "RTags" "*rdm*" (rtags-command))))
      (and rtags-autostart-diagnostics (rtags-diagnostics))
      (set-process-query-on-exit-flag rtags-rdm-process nil)
      (set-process-sentinel rtags-rdm-process 'rtags-sentinel)))))
(define-obsolete-function-alias 'rtags-start-process-maybe 'rtags-start-process-unless-running)

(defun rtags-sentinel (process event)
  "Watch the activity of RTags process (rdm)."
  (let ((status (process-status process)))
    (when (memq status '(exit signal closed failed))
      (message "rtags process (rdm) stopped..."))))

(defun rtags-completion-include-macros ()
  (unless (looking-back "\\.\\|->\\|::" (- (point) 2))
    "--code-complete-include-macros"))

(defconst rtags-symbol-chars "ABCDEFGHIKLMNOPQRSTUVWXYZabcdefghiklmnopqrstuvwxyz0123456789_")
(defun rtags-calculate-completion-point ()
  (save-excursion
    (when (cond ((= (point) (point-at-eol)))
                ((looking-at "[\\n A-Za-z0-9_)]"))
                ((looking-back "[\\n ,.:>A-Za-z0-9_(]" 1 t))
                (t nil))
      (when (= (skip-chars-backward " ") 0)
        (skip-chars-backward rtags-symbol-chars))
      (point))))

;;;###autoload
(defun rtags-reparse-file (&optional buffer periodic)
  "WAIT-REPARSING : t to wait for reparsing to finish, nil for async (no waiting)."
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (unless buffer
      (setq buffer (current-buffer)))
    (let ((file (rtags-buffer-file-name buffer)))
      ;;(when (null (rtags-buffer-status buffer))
      ;;(message ":debug: file not indexed"))
      (when (and file (rtags-buffer-status buffer))
        (if (buffer-modified-p buffer)
            (when (or rtags-enable-unsaved-reparsing periodic)
              (unless periodic
                (message "Reparsing %s" file))
              (rtags-call-rc :path file
                             :timeout rtags-reparse-timeout
                             :unsaved buffer
                             :silent t
                             "-V" file
                             (unless periodic "--wait")))
          (rtags-call-rc :path file
                         :silent t
                         "-V" file)
          (message (format "Dirtied %s" file)))))))

;; assoc list containing unsaved buffers and their modification ticks
;; (to avoid reparsing unsaved files if there were no changes since last parsing)
;; :fixme: - remove buffers from list on save
(defvar rtags-unsaved-buffer-ticks nil)
(make-variable-buffer-local 'rtags-unsaved-buffer-ticks)

(defun rtags-reparse-file-if-needed (&optional buffer periodic)
  "Reparse file if it's not saved.

buffer : The buffer to be checked and reparsed, if it's nil, use current buffer.
force means do it regardless of rtags-enable-unsaved-reparsing "
  (unless buffer
    (setq buffer (current-buffer)))
  (when (and (or rtags-enable-unsaved-reparsing periodic)
             (buffer-modified-p buffer)
             (funcall rtags-is-indexable buffer))
    ;; check ticks since the last save to avoid parsing the file multiple times
    ;; if it has not been modified
    (let ((current-ticks (buffer-modified-tick buffer))
          (old-ticks (buffer-local-value 'rtags-unsaved-buffer-ticks buffer)))
      ;; reparsing this dirty file for the first time
      ;; or if it was modified since last reparsing
      ;;(message ":debug: buffer=%s, old-ticks=%s, current-ticks=%s"
      ;;unsaved old-ticks current-ticks)
      (when (or (null old-ticks) (/= current-ticks old-ticks))
        (rtags-reparse-file buffer periodic)
        (set (make-local-variable 'rtags-unsaved-buffer-ticks) current-ticks)))))


;;;###autoload
(defun rtags-maybe-reparse-file (&optional buffer)
  (interactive)
  (let ((file (rtags-buffer-file-name buffer)))
    (when file
      (with-temp-buffer
        (rtags-call-rc :path file "-x" file)))))

(defun rtags-code-complete-enabled ()
  (and rtags-completions-enabled
       (memq major-mode rtags-supported-major-modes)))

(defconst rtags-paren-start ?()
(defconst rtags-paren-end ?))
(defun rtags-find-arg (startpos argument)
  (let ((location (cdr (assoc 'location argument))))
    (when (string-match ".*:\\([0-9]+\\):\\([0-9]+\\):?" location)
      (let* ((start (rtags-offset-for-line-column (string-to-number (match-string-no-properties 1 location))
                                                  (string-to-number (match-string-no-properties 2 location))))
             (end (and start (+ start (cdr (assoc 'length argument))))))
        (and start (>= startpos start) (< startpos end))))))

(defun* rtags-get-file-contents (&rest args
                                       &key
                                       (file nil)
                                       (startLine nil)
                                       (startColumn nil)
                                       (endLine nil)
                                       (endColumn nil)
                                       (location nil)
                                       (length nil)
                                       (maxlines nil)
                                       (info nil))
  (when (cond ((and location length (string-match "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)" location))
               (setq file (match-string-no-properties 1 location)
                     startLine (string-to-number (match-string-no-properties 2 location))
                     startColumn (string-to-number (match-string-no-properties 3 location)))
               (if maxlines
                   (error "maxlines doesn't work with location/length")
                 t))
              ((and startLine)
               (unless file
                 (setq file (rtags-buffer-file-name)))
               (unless endLine
                 (setq endLine startLine))
               (unless startColumn
                 (setq startColumn 1)))
              ((and info (let ((path (cdr (assoc 'location info)))
                               (sl (cdr (assoc 'startLine info)))
                               (sc (cdr (assoc 'startColumn info)))
                               (el (cdr (assoc 'endLine info)))
                               (ec (cdr (assoc 'endColumn info))))
                           (when (and sl sc el ec path (string-match "\\(.*\\):[0-9]+:[0-9]+" path))
                             (setq file (match-string-no-properties 1 path))
                             (setq startLine sl startColumn sc endLine el endColumn ec)))))
              (t nil))
    (let* ((file-or-buffer (rtags-trampify file))
           (buf (get-file-buffer file-or-buffer)))
      (unless buf
        (setq buf (find-file-noselect file-or-buffer)))
      (with-current-buffer buf
        (save-excursion
          (save-restriction
            (widen)
            (rtags-goto-line-col startLine startColumn)
            (let ((start (point)))
              (if length
                  (forward-char length)
                (rtags-goto-line-col endLine (or endColumn 1))
                (unless endColumn
                  (goto-char (point-at-eol))))
              (let ((ret (buffer-substring-no-properties start (point))))
                (when (and ret maxlines)
                  (let ((split (split-string ret "\n")))
                    (when (> (length split) maxlines)
                      (nbutlast split (- (length split) maxlines))
                      (setq ret (mapconcat 'identity split "\n")))))
                (and ret (list (cons 'contents ret)
                               (cons 'offset (rtags-offset start))))))))))))

(defun rtags--safe-substring (string &optional from to)
  (let ((len (length string)))
    (when (and from (< from 0))
      (setq from (+ len from)))
    (when (and to (< to 0))
      (setq to (+ len to)))
    (cond ((and from to (> from to)) "")
          ((and from (> from len)) "")
          ((and to (> to len)) "")
          (t (substring string from to)))))

(defun rtags-get-arg-usage-text (info)
  (when info
    (let* ((invokedFunction (rtags-symbol-info-internal :location (cdr (assoc 'invokedFunction info))))
           (invokedFunctionContents (and invokedFunction (rtags-get-file-contents :info invokedFunction :maxlines 1)))
           (invokedFunctionString (cdr (assoc 'contents invokedFunctionContents)))
           (functionArgument (rtags-get-file-contents :location (cdr (assoc 'functionArgumentLocation info))
                                                      :length (cdr (assoc 'functionArgumentLength info)))))
      (when (and functionArgument invokedFunctionContents)
        (concat (rtags--safe-substring invokedFunctionString 0 (- (cdr (assoc 'offset functionArgument))
                                                                  (cdr (assoc 'offset invokedFunctionContents))))
                (propertize (cdr (assoc 'contents functionArgument)) 'face 'rtags-argument-face)
                (rtags--safe-substring invokedFunctionString (+ (- (cdr (assoc 'offset functionArgument))
                                                                   (cdr (assoc 'offset invokedFunctionContents)))
                                                                (length (cdr (assoc 'contents functionArgument))))))))))

(defun rtags-get-summary-text (&optional max-num-lines)
  "Return a text describing the item at point.

For functions it is the declaration, including the parameters names, if available
or the first MAX-NUM-LINES (default 5) lines of the definition; for variables is
the definition, etc.

Return nil if it can't get any info about the item."
  ;; try first with --declaration-only
  (let ((symbol (rtags-symbol-info-internal :location (or (rtags-target-declaration-first) (rtags-current-location)) :silent t)))
    (when symbol
      (let ((brief (cdr (assoc 'briefComment symbol)))
            symbol-text
            (arg-text (rtags-get-arg-usage-text (rtags-symbol-info-internal))))
        (unless (> (length brief) 0)
          (setq brief nil))
        (if (string= (cdr (assoc 'kind symbol)) "EnumConstantDecl")
            (setq symbol-text (format "enum: %s = %d(0x%x)" (cdr (assoc 'symbolName symbol))
                                      (cdr (assoc 'enumValue symbol)) (cdr (assoc 'enumValue symbol))))
          (setq symbol-text (cdr (assoc 'contents (rtags-get-file-contents :info symbol :maxlines (or max-num-lines 5)))))
        (when arg-text
          (setq symbol-text (concat symbol-text "\n" arg-text)))
        (when brief
          (setq symbol-text (concat symbol-text "\n\n" brief)))
        symbol-text)))))

;;;###autoload
(defun rtags-display-summary (&optional hide-empty pos)
  "Display a short text describing the item at point.
See `rtags-get-summary-text' for details.
If `rtags-display-summary-as-tooltip' is t, a tooltip is displayed."
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (let (summary)
      (save-excursion
        (when pos (goto-char pos))
        (setq summary (rtags-get-summary-text))
        (when (or summary (not hide-empty))
          (when (null summary)
            (setq summary "No information for symbol"))))
      (when summary
        (if rtags-display-summary-as-tooltip
            (popup-tip summary :point pos)
          (message "%s" summary))))))

;;;###autoload
(defun rtags-display-summary-as-message ()
  "Display a short text in message area describing the item at point.
See `rtags-get-summary-text' for details."
  (interactive)
  (message "%s" (or (rtags-get-summary-text) "No information for symbol")))

(defun rtags-display-tooltip-function (event)
  (interactive)
  (when (and (funcall rtags-is-indexable (current-buffer))
             (eventp event))
    (let ((pos (posn-point (event-end event))))
      (when pos
        (rtags-display-summary t pos)
        t))))

(when rtags-tooltips-enabled
  (add-hook 'tooltip-functions 'rtags-display-tooltip-function))

(defun rtags-set-buffers (buffers)
  "Send the list of indexable buffers to the rtags server, rdm,
so it knows what files may be queried which helps with responsiveness.
"
  (when rtags-enabled
    (with-temp-buffer
      (mapc #'(lambda (x)
                (when (funcall rtags-is-indexable x)
                  (insert (rtags-buffer-file-name x) "\n")))
            buffers)
      (when (> (point-max) 1)
        (rtags-log (concat "--set-buffers files: "
                           (combine-and-quote-strings
                            (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n" t)))))
      (rtags-call-rc :noerror t :silent-query t :silent t :path t :unsaved (current-buffer) "--set-buffers" "-"))))

(defun rtags-kill-buffer-hook ()
  "When killing a buffer that is indexable, inform rdm of the new
set of buffers we are visiting."
  (when (and (rtags-buffer-file-name)
             (memq major-mode rtags-supported-major-modes))
    (unless (file-directory-p default-directory)
      (cd "/"))
    (rtags-set-buffers (remove (current-buffer) (buffer-list))))
  t)
(add-hook 'kill-buffer-hook 'rtags-kill-buffer-hook)

(defvar rtags-update-buffer-list-timer nil)
(defun rtags-update-buffer-list ()
  (interactive)
  ;; (message "rtags-update-buffer-list")
  (when rtags-update-buffer-list-timer
    (cancel-timer rtags-update-buffer-list-timer)
    (setq rtags-update-buffer-list-timer nil))
  (rtags-set-buffers (buffer-list)))

(defun rtags-schedule-buffer-list-update ()
  ;; (message "rtags-schedule-buffer-list-update %s" (if rtags-update-buffer-list-timer "yes" "no"))
  (unless rtags-update-buffer-list-timer
    (setq rtags-update-buffer-list-timer (run-with-idle-timer 1 nil #'rtags-update-buffer-list))))

(defun rtags-find-file-hook ()
  (interactive)
  (when (rtags-buffer-file-name)
    (rtags-schedule-buffer-list-update))
  t)
(add-hook 'find-file-hook 'rtags-find-file-hook)

(defun rtags-insert-include (include)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward include nil t)
        (message "\"%s\" is already included" include)
      (goto-char (point-min))
      (let ((head "\n")
            (tail "")
            (include (replace-regexp-in-string "\n$" "" include)))
        (if (re-search-forward "^# *include\\>" nil t)
            (end-of-line)
          (setq head "")
          (setq tail "\n")
          (goto-char (point-min)))
        (insert head include tail))
      (message "Added %s" include))))

;;;###autoload
(defun rtags-get-include-file-for-symbol (&optional prefix)
  "Insert #include declaration to buffer corresponding to the input symbol.
With optional PREFIX insert include at point."
  (interactive "P")
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (let* ((token (rtags-current-token))
           (prompt (if token
                       (format "Symbol (default: %s): " token)
                     "Symbol: "))
           (input (if (fboundp 'completing-read-default)
                      (completing-read-default prompt #'rtags-symbolname-complete nil nil nil 'rtags-symbol-history)
                    (completing-read prompt #'rtags-symbolname-complete nil nil nil 'rtags-symbol-history)))
           (current-file (rtags-buffer-file-name)))
      (setq rtags-symbol-history (rtags-remove-last-if-duplicated rtags-symbol-history))
      (when (string= "" input)
        (if token
            (setq input token)
          (message "You entered an empty symbol. Try again.")))
      (let ((include (with-temp-buffer
                       (rtags-call-rc :path current-file
                                      "--include-file" input
                                      (when rtags-symbolnames-case-insensitive "-I"))
                       (cond ((= (point-min) (point-max))
                              (message "RTags: No results") nil)
                             ((= (count-lines (point-min) (point-max)) 1)
                              (buffer-substring-no-properties (point-min) (1- (point-max))))
                             (t
                              ;; (message "Results:\n%s" (buffer-substring-no-properties (point-min) (point-max)))
                              (completing-read "Choose: " (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n" t) nil t))))))
        (when include
          (if prefix
              (insert include)
            (rtags-insert-include include)))))))

(defun rtags-include-file (&optional prefix)
  "Insert selected or entered include name in buffer.
With optional PREFIX insert include at point."
  (interactive "P")
  (let* ((alternatives (let ((buf (or (rtags-buffer-file-name) (error "Buffer is not visiting a file"))))
                         (with-temp-buffer (rtags-call-rc :path buf
                                                          "--code-complete-at" (concat buf ":1:1:")
                                                          "--code-complete-includes"
                                                          "--elisp")
                                           (goto-char (point-min))
                                           (point-max)
                                           (and (looking-at "(")
                                                (eval (read (buffer-string)))))))
         (file (and alternatives (completing-read "File: "
                                                  (let ((all))
                                                    (mapc (lambda (include)
                                                            (push (format (format "<%s>" include)) all)
                                                            (push (format (format "\"%s\"" include)) all))
                                                          alternatives)
                                                    all)))))
    (unless alternatives
      (error "No valid includes found"))
    (when file
      (let ((include (concat "#include " file)))
        (if prefix
            (insert include)
          (rtags-insert-include include))))))

(defun rtags-real-target (info)
  (let* ((kind (cdr (assoc 'kind info)))
         (ret)
         (targets (and kind (cdr (assoc 'targets info)))))
    (while targets
      (let ((targetkind (cdr (assoc 'kind (car targets)))))
        (if (and targetkind (string= targetkind kind))
            (setq ret (car targets)
                  targets nil)
          (setq targets (cdr targets)))))
    ret))

(defun rtags-range-for-symbol-info (info)
  (save-excursion
    (let ((start (and (rtags-goto-line-col (cdr (assoc 'startLine info))
                                           (cdr (assoc 'startColumn info)))
                      (+ (point) (length (cdr (assoc 'symbolName info))))))
          (end (and (rtags-goto-line-col (cdr (assoc 'endLine info))
                                         (cdr (assoc 'endColumn info)))
                    (point))))
      (and start end (cons start end)))))

(defun rtags-peer-member-end-location ()
  (let ((sym (rtags-symbol-info-internal)))
    ;; (message "TRYING %s => %s" (rtags-current-location) (if sym "yes" "no"))
    (when (cond ((not sym) nil)
                ((string= (cdr (assoc 'kind sym)) "CXXConstructor"))
                ((string= (cdr (assoc 'kind sym)) "CXXDestructor"))
                ((string= (cdr (assoc 'kind sym)) "CXXMethod"))
                (t nil))
      (let ((target (rtags-real-target sym)))
        (when target
          (let ((location (cdr (assoc 'location target))))
            (when (string-match "\\(.*\\):[0-9]+:[0-9]+:?" location)
              (let ((buffer (find-file-noselect (match-string-no-properties 1 location))))
                ;; (message "GOT target %s for %s (%d:%d)" location (rtags-current-location)
                ;; (cdr (assoc 'endLine target))
                ;; (cdr (assoc 'endColumn target)))
                (with-current-buffer buffer
                  (rtags-goto-line-col (cdr (assoc 'endLine target))
                                       (cdr (assoc 'endColumn target)))
                  (cons buffer (1+ (point))))))))))))

(defun rtags-find-location-for-function (range)
  (let (loc)
    (save-excursion
      (while (and (not loc)
                  (> (point) (car range)))
        (backward-word)
        (setq loc (rtags-peer-member-end-location))))
    (save-excursion
      (while (and (not loc)
                  (< (point) (cdr range)))
        ;; forward-word puts us at the end of each symbol
        (forward-char 1)
        (forward-word)
        (forward-char -1)
        (setq loc (rtags-peer-member-end-location))))
    (unless loc
      ;; Need to handle creation of a buffer with appropriate name
      (if (member (downcase (file-name-extension (rtags-buffer-file-name))) (list "c" "cxx" "cc" "cpp" "c++"))
          (setq loc (cons (current-buffer) (+ (cdr range) 2)))
        (let ((fn (rtags-buffer-file-name)))
          (find-file (concat (file-name-sans-extension fn) ".cpp"))
          (insert "#include \"" (file-name-nondirectory fn) "\"\n")
          (setq loc (cons (current-buffer) (point))))))
    loc))

(defun rtags-stack-cost ()
  (interactive)
  (let* ((container (rtags-current-container))
         (kind (cdr (assoc 'kind container))))
    (unless (member kind (list "CXXConstructor" "CXXDestructor" "CXXMethod" "FunctionDecl" "FunctionTemplate" "LambdaExpr"))
      (error "Can't find a function here"))
    (when (rtags-called-interactively-p)
      (message "Current function: %s stackCost: %d" (cdr (assoc 'symbolName container)) (cdr (assoc 'stackCost container))))
    (cdr (assoc 'stackSize container))))

(defun rtags-find-member-function ()
  (save-excursion
    (let ((start (point))
          (start (point-at-bol))
          (valid (list "CXXMethod" "CXXConstructor" "CXXDestructor" "FunctionTemplate"))
          (sym (rtags-symbol-info-internal :silent t :parents t)))
      (unless (and sym (member (cdr (assoc 'kind sym)) valid))
        (goto-char (point-at-eol))
        (while (and (not sym) (>= (point) start))
          (setq sym (rtags-symbol-info-internal :silent t))
          (unless (and sym (member (cdr (assoc 'kind sym)) valid))
            (setq sym nil)
            (backward-word))))
      sym)))

;;;###autoload
(defun rtags-make-member ()
  "Create a stub member functions. Type a declaration and then
`rtags-make-member' can be used to create the stub definition in
the class.
"
  (interactive)
  (let* ((member (rtags-find-member-function))
         (parent (cdr (assoc 'parent member)))
         (kind (cdr (assoc 'kind member))))
    (unless (and kind (member (cdr (assoc 'kind parent)) (list "ClassDecl" "StructDecl" "ClassTemplate")))
      (error "No appropriate symbol here that I know of"))
    (when (cdr (assoc 'definition member))
      (error "This is already the definition"))
    (when (rtags-real-target member)
      (error "%s is already implemented here: %s"
             (cdr (assoc 'symbolName member))
             (cdr (assoc 'location (rtags-real-target member)))))
    (let ((range (rtags-range-for-symbol-info parent)))
      (unless range
        (error "Can't find the range"))
      (let ((loc (rtags-find-location-for-function range)))
        (unless loc
          (error "Can't find a location for this function"))
        (rtags-switch-to-buffer (car loc))
        (goto-char (cdr loc))
        (insert "\n" (cdr (assoc 'symbolName member)) "\n{")
        (save-excursion
          (insert "}\n")
          (unless (eobp)
            (insert "\n")))))))

(defvar rtags-check-includes-received-output nil)
(defun rtags-check-includes-filter (process output)
  (with-current-buffer (process-buffer process)
    (let ((buffer-read-only nil))
      (unless rtags-check-includes-received-output
        (setq rtags-check-includes-received-output t)
        (erase-buffer))
      (goto-char (point-max))
      (insert output))))

(defun rtags-check-includes-sentinel (process event)
  (let ((status (process-status process)))
    (when (memq status '(exit signal closed failed))
      (goto-char (point-min)))))

;;;###autoload
(defun rtags-check-includes ()
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (let ((filename (rtags-untrampify (rtags-buffer-file-name)))
          (rc (rtags-executable-find "rc")))
      (unless rc
        (error "Can't find rc"))
      (unless filename
        (error "You need to call rtags-check-includes from an actual file"))
      (rtags-switch-to-buffer (rtags-get-buffer "*RTags check includes*"))
      (rtags-mode)
      (set (make-local-variable 'rtags-check-includes-received-output) nil)
      (let ((buffer-read-only nil))
        (insert "Waiting for rdm..."))
      (goto-char (point-min))
      (let ((proc (start-file-process "*RTags check includes*"
                                      (current-buffer)
                                      rc
                                      "--current-file" filename
                                      "--check-includes" filename)))
        (set-process-query-on-exit-flag proc nil)
        (set-process-filter proc 'rtags-check-includes-filter)
        (set-process-sentinel proc 'rtags-check-includes-sentinel)))))


(defvar rtags-tokens-callback nil)
(make-variable-buffer-local 'rtags-tokens-callback)

(defun rtags-tokens-sentinel (process event)
  (let ((status (process-status process)))
    (when (memq status '(exit signal closed failed))
      (with-current-buffer (process-buffer process)
        (goto-char (point-min))
        (funcall rtags-tokens-callback (and (looking-at "(")
                                            (eval (read (buffer-string)))))
        (kill-buffer (process-buffer process))))))

;;;###autoload
(defun rtags-tokens (&optional from to callback)
  (interactive)
  (when (and (not from)
             (not to)
             mark-active)
    (setq from (region-beginning))
    (setq to (region-end)))
  (when (and from to)
    (let ((min (min from to))
          (max (max from to)))
      (setq from min)
      (setq to max)))
  (let ((path (rtags-buffer-file-name)))
    (unless path
      (error "rtags-tokens must be run from a buffer visiting a file"))
    (cond ((functionp callback)
           (let ((buf (rtags-get-buffer-create-no-undo " *RTags Tokens*")))
             (with-current-buffer buf
               (erase-buffer)
               (let ((proc (start-process "RTags Tokens Async"
                                          buf
                                          (rtags-executable-find "rc")
                                          "--elisp"
                                          "--tokens-include-symbols"
                                          "--tokens" (cond ((and from to) (format "%s:%d-%d" path from to))
                                                           (from (format "%s:%d-" path from))
                                                           (to (format "%s:-%d" path to))
                                                           (t path)))))
                 (setq rtags-tokens-callback callback)
                 (set-process-sentinel proc 'rtags-tokens-sentinel)))))
          ((null callback)
           (with-temp-buffer
             (rtags-call-rc :path path
                            "--elisp"
                            "--tokens-include-symbols"
                            "--tokens" (cond ((and from to) (format "%s:%d-%d" path from to))
                                             (from (format "%s:%d-" path from))
                                             (to (format "%s:-%d" path to))
                                             (t path)))
             (and (looking-at "(")
                  (eval (read (buffer-string))))))
          (t (error "Callback must be a function")))))

;;;###autoload
(defun rtags-create-doxygen-comment ()
  "Creates doxygen comment for function at point.

Comment will be inserted before current line. It uses yasnippet to let
the user enter missing field manually."
  (interactive)
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (save-some-buffers) ;; it all kinda falls apart when buffers are unsaved
    (let ((symbol (rtags-symbol-info-internal)))
      (unless symbol
        (error "Can't find symbol here"))
      (let* ((type (cdr (assoc 'type symbol)))
             (return-val (and (string-match "^\\([^)]*\\) (.*" type)
                              (match-string 1 type)))
             ;;           (args (mapcar (lambda (arg) (cdr (assoc 'symbolName arg))) (cdr (assoc 'arguments symbol))))
             (index 2)
             (snippet (concat "/** @Brief ${1:Function description}\n"
                              (mapconcat #'(lambda (argLoc)
                                             (let* ((arg (rtags-symbol-info-internal :location (cdr (assoc 'cursor argLoc))))
                                                    (complete-name (cdr (assoc 'symbolName arg)))
                                                    (symbol-type (cdr (assoc 'type arg)))
                                                    (symbol-name (substring complete-name (- 0 (cdr (assoc 'symbolLength arg)))))
                                                    (ret (format " * @param %s <b>{%s}</b> ${%d:Parameter description}"
                                                                 symbol-name symbol-type index)))
                                               (incf index)
                                               ret))
                                         (cdr (assoc 'arguments symbol))
                                         "\n")
                              (unless (string= return-val "void")
                                (format "%s * @return <b>{%s}</b> ${%d:Return value description}\n"
                                        (if (eq index 2)
                                            ""
                                          "\n")
                                        return-val index))
                              " */\n")))
        (beginning-of-line)
        (yas-expand-snippet snippet (point) (point) nil)))))

(defun rtags-eldoc ()
  (when (and (not (nth 4 (syntax-ppss)))
             (let ((text (thing-at-point 'symbol)))
               (when (and text (sequencep text))
                 (set-text-properties 0 (length text) nil text))
               text))
    (let ((doc (rtags-get-summary-text)))
      (and doc
           (replace-regexp-in-string
            "{.*" ""
            (replace-regexp-in-string
             "[ \t\n]+" " "
             (replace-regexp-in-string "\n" " " doc)))))))

(provide 'rtags)

;;; rtags.el ends here
