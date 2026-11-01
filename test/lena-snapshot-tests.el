;;; lena-snapshot-tests.el --- Tests for lena-snapshot  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see lena/LICENSE.GPL3.

;;; Commentary:

;; Tests for `lena-snapshot' that need no Lean server: snapshots of a
;; Lena buffer that Eglot does not manage.

;;; Code:

(require 'ert)
(require 'lena-snapshot)

(defmacro lena-snapshot-test-with-buffer (&rest body)
  "Run BODY with a Lena buffer `source' and a fresh snapshot directory.
The buffer visits a file `Test.lean' in a temporary directory, which is
also `lena-snapshot-directory'."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "lena-snapshot-test" t))
          (lena-snapshot-directory root)
          (file (expand-file-name "Test.lean" root)))
     (with-temp-file file (insert "theorem t : True := trivial\n"))
     (let ((source (find-file-noselect file)))
       (unwind-protect
           (with-current-buffer source ,@body)
         (with-current-buffer source (set-buffer-modified-p nil))
         (kill-buffer source)
         (delete-directory root t)))))

(defun lena-snapshot-test-read (directory file)
  "Return the contents of FILE in the snapshot DIRECTORY."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file directory))
    (buffer-string)))

(defun lena-snapshot-test-call (answer)
  "Call `lena-snapshot' interactively; return the snapshot's directory.
ANSWER is called instead of `read-string' to answer its question."
  (cl-letf (((symbol-function 'read-string) answer))
    (call-interactively #'lena-snapshot)))

(ert-deftest lena-snapshot-test-state-first ()
  "The state is recorded before the question, whose answer is the note."
  (lena-snapshot-test-with-buffer
    (let* ((directory
            (lena-snapshot-test-call
             (lambda (&rest _)
               ;; A change while the note is typed.
               (with-current-buffer source
                 (goto-char (point-max))
                 (insert "-- typed later\n"))
               "Something looks wrong")))
           (org (lena-snapshot-test-read directory "snapshot.org")))
      (should (equal (lena-snapshot-test-read directory "source.lean")
                     "theorem t : True := trivial\n"))
      (should (string-prefix-p "#+title: Something looks wrong\n" org))
      (should (string-search "* Note\nSomething looks wrong\n" org))
      (should (string-search "\n- Major mode: lena-mode\n" org))
      ;; With the live tests, a server manages the buffer too.
      (should (string-search "\n* Server\n- Server: " org))
      (should (equal (car kill-ring) directory)))))

(ert-deftest lena-snapshot-test-quit ()
  "Quitting the question keeps the snapshot, without a note."
  (lena-snapshot-test-with-buffer
    (let* ((directory (lena-snapshot-test-call
                       (lambda (&rest _) (signal 'quit nil))))
           (org (lena-snapshot-test-read directory "snapshot.org")))
      (should (string-prefix-p "#+title: Snapshot\n" org))
      (should (string-search "* Note\nNone: the question was quit.\n" org))
      (should (string-search "* Buffer\n" org)))))

(ert-deftest lena-snapshot-test-note-argument ()
  "Called with a note, it does not ask."
  (lena-snapshot-test-with-buffer
    (let* ((directory (cl-letf (((symbol-function 'read-string)
                                 (lambda (&rest _) (error "Asked"))))
                        (lena-snapshot "Given")))
           (org (lena-snapshot-test-read directory "snapshot.org")))
      (should (string-prefix-p "#+title: Given\n" org)))))

(ert-deftest lena-snapshot-test-field-unavailable ()
  "A field whose internal fails says so, and the rest is recorded."
  (lena-snapshot-test-with-buffer
    (cl-letf (((symbol-function 'lena-eglot-compat-recent-changes)
               (lambda () (error "Changed in Eglot"))))
      (let ((org (lena-snapshot-test-read (lena-snapshot "Note")
                                          "snapshot.org")))
        (should (string-search
                 "\n- Changes not sent: unavailable: Changed in Eglot\n"
                 org))
        (should (string-search "\n- Semantic tokens in the buffer" org))
        (should (string-search "* Around\n" org))))))

(ert-deftest lena-snapshot-test-version ()
  "Without a checkout of its own, Lena's version is that of its header."
  (lena-snapshot-test-with-buffer
    (cl-letf (((symbol-function 'lena-snapshot--git) #'ignore))
      (should (equal (alist-get "Lena" (lena-snapshot--versions)
                                nil nil #'equal)
                     (lena-snapshot--library-version "lena"))))))

(ert-deftest lena-snapshot-test-events-log ()
  "A snapshot says how much of Eglot's events it can have."
  (let ((eglot-events-buffer-config '(:size 0 :format full)))
    (should (string-prefix-p "none" (lena-snapshot--events-log))))
  (let ((eglot-events-buffer-config '(:size 2000000 :format full)))
    (should (string-prefix-p "the last 2000000 characters"
                             (lena-snapshot--events-log))))
  (let ((eglot-events-buffer-config '(:size nil :format full)))
    (should (equal (lena-snapshot--events-log) "all"))))

(ert-deftest lena-snapshot-test-new-directory ()
  "Snapshots in the same second get directories of their own."
  (lena-snapshot-test-with-buffer
    (let* ((time (current-time))
           (first (lena-snapshot--new-directory time)))
      (make-directory first)
      (should-not (equal (lena-snapshot--new-directory time) first))
      (should (string-suffix-p "-2/" (lena-snapshot--new-directory time))))))

(provide 'lena-snapshot-tests)

;;; lena-snapshot-tests.el ends here
