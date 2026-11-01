;;; lena-snapshot.el --- Record Lena's state when something looks wrong  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see LICENSE.GPL3.

;;; Commentary:

;; `lena-snapshot' records what Lena, Eglot and the Lean server are doing
;; at that moment, and then what looks wrong, in a directory of its own
;; under `lena-snapshot-directory'.  It is meant for problems that are
;; gone, or that a restart makes go away, by the time one looks into
;; them, so it is best called before restarting anything.
;;
;; Much of what it records are internals of Eglot, read through
;; `lena-eglot-compat'.  Each is recorded on its own, so that one that
;; changed leaves a note in its place rather than no snapshot.
;;
;; The most useful part of a snapshot is Eglot's events buffer, which
;; `eglot-events-buffer-config' limits to 2 MB by default, and which
;; some configurations, such as Doom Emacs, turn off.  Lean fills 2 MB
;; within minutes, so make it larger:
;;
;;   (with-eval-after-load 'eglot
;;     (setq eglot-events-buffer-config '(:size 50000000 :format full)))

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'lisp-mnt)
(require 'seq)

(require 'lena-eglot)
(require 'lena-eglot-compat)

(defvar lena-rpc--sessions)

(defcustom lena-snapshot-directory (locate-user-emacs-file "lena-snapshots/")
  "Directory in which `lena-snapshot' makes a directory per snapshot."
  :type 'directory
  :group 'lena)

(defcustom lena-snapshot-messages-lines 100
  "Number of lines at the end of *Messages* that a snapshot keeps."
  :type 'natnum
  :group 'lena)

(defmacro lena-snapshot--field (label value)
  "Return (LABEL . VALUE), or why VALUE could not be had in its place.
A field fails when an internal that it reads changed, which should not
keep the rest of the snapshot from being written."
  (declare (indent 1) (debug t))
  `(cons ,label (condition-case err
                    ,value
                  (error (format "unavailable: %s"
                                 (error-message-string err))))))

(defun lena-snapshot--source-buffer ()
  "Return the Lena buffer that a snapshot is about, or nil.
That is the current buffer if it is a Lena buffer, else the one most
recently selected."
  (seq-find (lambda (buffer)
              (with-current-buffer buffer (derived-mode-p 'lena-mode)))
            (cons (current-buffer) (buffer-list))))

(defun lena-snapshot--directory ()
  "Return the directory that Lena is loaded from, or nil."
  (when-let* ((library (locate-library "lena")))
    (file-name-directory (file-truename library))))

(defun lena-snapshot--git (&rest args)
  "Return the output of git with ARGS in Lena's repository, or nil.
Nil also when Lena is not loaded from a checkout of its own, as when it
is installed as a package."
  (when-let* ((default-directory (lena-snapshot--directory)))
    (cl-flet ((git (&rest args)
                (with-temp-buffer
                  (when (eq 0 (apply #'process-file "git" nil t nil args))
                    (string-trim (buffer-string))))))
      (when-let* ((top (git "rev-parse" "--show-toplevel"))
                  ((file-exists-p (expand-file-name "lena/lena.el" top))))
        (apply #'git args)))))

(defun lena-snapshot--library-version (library)
  "Return the version in the header of LIBRARY's source, or nil."
  (when-let* ((file (or (locate-library (concat library ".el") t)
                        (locate-library (concat library ".el.gz") t))))
    (lm-version file)))

(defun lena-snapshot--toolchain (directory)
  "Return the contents of the `lean-toolchain' for DIRECTORY, or nil."
  (when-let* ((found (locate-dominating-file directory "lean-toolchain")))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "lean-toolchain" found))
      (string-trim (buffer-string)))))

(defun lena-snapshot--semtok-state ()
  "Return Eglot's state of the semantic tokens of the current buffer, shortened.
The tokens and the regions to fontify are replaced by their number."
  (let ((state (copy-sequence (lena-eglot-compat-semantic-tokens-state))))
    (when (plist-member state :data)
      (setq state (plist-put state :data
                             (format "<%d numbers>"
                                     (length (plist-get state :data))))))
    (when (plist-member state :regions)
      (setq state (plist-put state :regions
                             (format "<%d regions>"
                                     (length (plist-get state :regions))))))
    state))

(defun lena-snapshot--semtok-runs (beg end)
  "Return how the semantic tokens between BEG and END are painted.
The value is a list (TOKENS PAINTED): the number of runs of text that
have tokens, and of those that also have a face of a semantic token."
  (let ((tokens 0) (painted 0) (pos beg))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'eglot--semtok-faces nil end)))
        (when (get-text-property pos 'eglot--semtok-faces)
          (cl-incf tokens)
          (when (seq-some (lambda (face)
                            (and (symbolp face)
                                 (string-prefix-p "eglot-semantic-"
                                                  (symbol-name face))))
                          (ensure-list (get-text-property pos 'face)))
            (cl-incf painted)))
        (setq pos next)))
    (list tokens painted)))

(defun lena-snapshot--where (window)
  "Return where point is in the current buffer, shown in WINDOW or not."
  (list
   (lena-snapshot--field "File" (or buffer-file-name (buffer-name)))
   (lena-snapshot--field "Point"
     (format "line %d, column %d (position %d)"
             (line-number-at-pos) (current-column) (point)))
   (lena-snapshot--field "Window"
     (if window
         (format "lines %d to %d"
                 (line-number-at-pos (window-start window))
                 (line-number-at-pos (window-end window t)))
       "not shown"))))

(defun lena-snapshot--versions ()
  "Return the versions of Emacs, its packages, Lena and Lean."
  (list
   (lena-snapshot--field "Emacs" emacs-version)
   (lena-snapshot--field "Eglot" (lena-snapshot--library-version "eglot"))
   (lena-snapshot--field "jsonrpc" (lena-snapshot--library-version "jsonrpc"))
   (lena-snapshot--field "Flymake" (lena-snapshot--library-version "flymake"))
   (lena-snapshot--field "Lena"
     (let ((version (lena-snapshot--library-version "lena"))
           (describe (lena-snapshot--git "describe" "--always" "--dirty"))
           (branch (lena-snapshot--git "rev-parse" "--abbrev-ref" "HEAD")))
       (if describe
           (format "%s, %s on %s" version describe branch)
         version)))
   (lena-snapshot--field "Lena's directory" (lena-snapshot--directory))
   (lena-snapshot--field "lean-toolchain"
     (lena-snapshot--toolchain default-directory))))

(defun lena-snapshot--buffer-state (window)
  "Return the state of the current buffer, shown in WINDOW or not."
  (list
   (lena-snapshot--field "Major mode" major-mode)
   (lena-snapshot--field "eglot--managed-mode"
     (bound-and-true-p eglot--managed-mode))
   (lena-snapshot--field "eglot-semantic-tokens-mode"
     (bound-and-true-p eglot-semantic-tokens-mode))
   (lena-snapshot--field "eglot-inlay-hints-mode"
     (bound-and-true-p eglot-inlay-hints-mode))
   (lena-snapshot--field "font-lock-mode" (bound-and-true-p font-lock-mode))
   (lena-snapshot--field "jit-lock-mode" (bound-and-true-p jit-lock-mode))
   (lena-snapshot--field "flymake-mode" (bound-and-true-p flymake-mode))
   (lena-snapshot--field "lena-margin-mode"
     (bound-and-true-p lena-margin-mode))
   (lena-snapshot--field "Modified" (buffer-modified-p))
   (lena-snapshot--field "Same as the file on disk"
     (and buffer-file-name (verify-visited-file-modtime)))
   (lena-snapshot--field "Document version"
     (lena-eglot-compat-document-version))
   (lena-snapshot--field "Changes not sent"
     (let ((changes (lena-eglot-compat-recent-changes)))
       (if (listp changes)
           (format "<%d changes>" (length changes))
         changes)))
   (lena-snapshot--field "Semantic tokens" (lena-snapshot--semtok-state))
   (lena-snapshot--field "Semantic tokens in the window (runs, painted)"
     (and window
          (lena-snapshot--semtok-runs (window-start window)
                                      (window-end window t))))
   (lena-snapshot--field "Semantic tokens in the buffer (runs, painted)"
     (save-restriction
       (widen)
       (lena-snapshot--semtok-runs (point-min) (point-max))))
   (lena-snapshot--field "lena-eglot-file-progress" lena-eglot-file-progress)
   (lena-snapshot--field "Diagnostics (line, severity, first line)"
     (mapcar (lambda (diagnostic)
               (list (1+ (car (lena-eglot-diagnostic-lines diagnostic)))
                     (plist-get diagnostic :severity)
                     (car (split-string (plist-get diagnostic :message)
                                        "\n"))))
             lena-eglot-diagnostics))))

(defun lena-snapshot--server-state (server)
  "Return the state of the Eglot SERVER."
  (if (not server)
      (list (cons "Server" "none"))
    (list
     (lena-snapshot--field "Server" (jsonrpc-name server))
     (lena-snapshot--field "Running" (and (jsonrpc-running-p server) t))
     (lena-snapshot--field "Process"
       (let ((process (lena-eglot-compat-process server)))
         (and (process-live-p process)
              (format "%d: %s" (process-id process)
                      (string-join (process-command process) " ")))))
     (lena-snapshot--field "Buffers"
       (mapcar #'buffer-name (lena-eglot-compat-managed-buffers server)))
     (lena-snapshot--field "Pending requests"
       (lena-eglot-compat-pending-requests server))
     (lena-snapshot--field "Last error" (jsonrpc-last-error server))
     (lena-snapshot--field "semanticTokensProvider"
       (plist-get (lena-eglot-compat-capabilities server)
                  :semanticTokensProvider)))))

(defun lena-snapshot--events-log ()
  "Return how much of Eglot's events a snapshot can have."
  (let ((size (plist-get eglot-events-buffer-config :size)))
    (cond ((eql size 0)
           "none, since it is off; see `eglot-events-buffer-config'")
          ((natnump size)
           (format "the last %d characters; see `eglot-events-buffer-config'"
                   size))
          (t "all"))))

(defun lena-snapshot--other-state ()
  "Return the state of everything around the buffer."
  (list
   (lena-snapshot--field "Eglot servers"
     (mapcar #'jsonrpc-name (lena-eglot-compat-servers)))
   (lena-snapshot--field "Eglot's events log" (lena-snapshot--events-log))
   (lena-snapshot--field "RPC sessions"
     (and (boundp 'lena-rpc--sessions)
          (hash-table-count lena-rpc--sessions)))
   (lena-snapshot--field "Infoview"
     (let ((buffer (get-buffer "*Lena Infoview*")))
       (cond ((not buffer) "none")
             ((get-buffer-window buffer t) "shown")
             (t "hidden"))))))

(defun lena-snapshot--insert-section (title items)
  "Insert an Org section TITLE that lists ITEMS, an alist."
  (insert "* " title "\n")
  (pcase-dolist (`(,label . ,value) items)
    (insert "- " label ": "
            (if (stringp value) value (format "%S" value))
            "\n"))
  (insert "\n"))

(defun lena-snapshot--file-name (name)
  "Return NAME, a buffer name, as a file name."
  (concat (string-trim (replace-regexp-in-string "[^[:alnum:]._-]+" "-" name)
                       "-+" "-+")
          ".log"))

(defun lena-snapshot--write-logs (directory)
  "Write Eglot's events and stderr buffers to DIRECTORY.
Return the names of the files."
  (let (files)
    (dolist (buffer (buffer-list))
      ;; The stderr buffer's name starts with a space, which hides it.
      (when (string-match-p (rx bos (? " ") "*EGLOT " (* nonl)
                                (or "events" "stderr") "*" eos)
                            (buffer-name buffer))
        (let ((file (lena-snapshot--file-name (buffer-name buffer))))
          (with-current-buffer buffer
            (save-restriction
              (widen)
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region nil nil (expand-file-name file directory)
                              nil 'silent))))
          (push file files))))
    (nreverse files)))

(defun lena-snapshot--messages ()
  "Return the last `lena-snapshot-messages-lines' lines of *Messages*."
  (with-current-buffer (messages-buffer)
    (save-excursion
      (goto-char (point-max))
      (forward-line (- lena-snapshot-messages-lines))
      (buffer-substring-no-properties (point) (point-max)))))

(defun lena-snapshot--new-directory (time)
  "Return the name of a directory for a snapshot at TIME that does not exist."
  (let* ((base (expand-file-name (format-time-string "%Y-%m-%dT%H%M%S" time)
                                 lena-snapshot-directory))
         (directory base)
         (n 1))
    (while (file-exists-p directory)
      (setq directory (format "%s-%d" base (cl-incf n))))
    (file-name-as-directory directory)))

;;;###autoload
(defun lena-snapshot (&optional note)
  "Record the state of Lena right now, and NOTE on what looks wrong.
The snapshot is a directory under `lena-snapshot-directory' with an Org
file that holds NOTE, the versions and the state of the Lena buffer and
its server, the end of *Messages*, the contents of the buffer, and
Eglot's events and stderr buffers.  Its name is copied to the kill ring.

Interactively, ask for NOTE only once the state is recorded, so that the
snapshot shows the state when the command was called.  Quitting the
question keeps the snapshot, without a note.

A snapshot holds the contents of the buffer, and Eglot's logs hold those
of other files that the server opened.  Look through it before sharing
it."
  (interactive)
  (let* ((called-from (current-buffer))
         (source (or (lena-snapshot--source-buffer)
                     (user-error "There is no Lena buffer")))
         (time (current-time))
         (directory (lena-snapshot--new-directory time))
         (source-file (concat "source."
                              (or (file-name-extension
                                   (or (buffer-file-name source) ""))
                                  "lean")))
         org)
    (make-directory directory t)
    (with-current-buffer source
      (let ((window (get-buffer-window source t))
            (server (eglot-current-server)))
        (save-excursion
          (when window (goto-char (window-point window)))
          (setq org
                (with-temp-buffer
                  (lena-snapshot--insert-section
                   "Where"
                   (cons (cons "Called from" (buffer-name called-from))
                         (with-current-buffer source
                           (lena-snapshot--where window))))
                  (lena-snapshot--insert-section
                   "Versions" (with-current-buffer source
                                (lena-snapshot--versions)))
                  (lena-snapshot--insert-section
                   "Buffer" (with-current-buffer source
                              (lena-snapshot--buffer-state window)))
                  (lena-snapshot--insert-section
                   "Server" (with-current-buffer source
                              (lena-snapshot--server-state server)))
                  (lena-snapshot--insert-section
                   "Around" (lena-snapshot--other-state))
                  (buffer-string))))
        (save-restriction
          (widen)
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region nil nil (expand-file-name source-file directory)
                          nil 'silent)))))
    (let ((logs (lena-snapshot--write-logs directory)))
      (unless note
        (setq note (condition-case nil
                       (read-string "What looks wrong? ")
                     (quit nil))))
      (with-temp-file (expand-file-name "snapshot.org" directory)
        (insert "#+title: "
                (if (or (null note) (string-empty-p note)) "Snapshot" note)
                "\n"
                (format-time-string "#+date: [%Y-%m-%d %a %H:%M:%S]\n\n" time)
                "* Note\n"
                (or note "None: the question was quit.")
                "\n\n"
                org)
        (insert "* Files\n")
        (dolist (file (cons source-file logs))
          (insert (format "- [[file:%s]]\n" file)))
        (insert "\n* Messages\n#+begin_example\n"
                (lena-snapshot--messages)
                "#+end_example\n")))
    (kill-new directory)
    (message "Lena snapshot in %s" directory)
    directory))

(provide 'lena-snapshot)

;;; lena-snapshot.el ends here
