;;; lena-indent-tests.el --- Tests for Lena's indentation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see lena/LICENSE.GPL3.

;;; Commentary:

;; Tests for `lena-indent-line' and its keys.  They need no Lean server.

;;; Code:

(require 'ert)
(require 'lena)

(defmacro lena-indent-test-with-buffer (text &rest body)
  "Run BODY in a Lena buffer holding TEXT, with point at its end.
`electric-indent-mode' is on, as in a default Emacs."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (lena-mode)
     (electric-indent-local-mode 1)
     (insert ,text)
     (let ((this-command nil)
           (last-command nil))
       ,@body)))

(defun lena-indent-test-return ()
  "Press RET."
  (setq this-command 'newline)
  (call-interactively #'newline)
  (run-hooks 'post-self-insert-hook))

(defun lena-indent-test-tab ()
  "Press TAB."
  (setq this-command 'indent-for-tab-command)
  (call-interactively #'indent-for-tab-command)
  (setq last-command this-command))

(defun lena-indent-test-line ()
  "Return the current line up to point."
  (buffer-substring-no-properties (line-beginning-position) (point)))

(ert-deftest lena-indent-test-keys ()
  "TAB indents, S-TAB dedents, and tabs are never inserted."
  (lena-indent-test-with-buffer ""
    (should (eq indent-line-function #'lena-indent-line))
    (should-not indent-tabs-mode)
    (should (eq (key-binding (kbd "<backtab>")) #'lena-indent-dedent))))

(ert-deftest lena-indent-test-return ()
  "RET indents the new line like the previous one, or deeper after a block."
  (pcase-dolist (`(,text . ,indentation)
                 '(("theorem foo : True := by" . "  ")
                   ("theorem foo : True := by -- comment" . "  ")
                   ("theorem foo : True := by\n  trivial" . "  ")
                   ("def f : Nat :=" . "  ")
                   ("  constructor\n  · intro h" . "    ")
                   ("  cases h with\n  | inl h =>" . "    ")
                   ("  cases h with\n  | inl h => exact h" . "  ")
                   ("" . "")))
    (lena-indent-test-with-buffer text
      (lena-indent-test-return)
      (should (equal (lena-indent-test-line) indentation)))))

(ert-deftest lena-indent-test-return-after-blank-line ()
  "After a blank line, RET starts at column 0: a new declaration."
  (lena-indent-test-with-buffer "theorem test : True := by\n  sorry"
    (lena-indent-test-return)
    (should (equal (lena-indent-test-line) "  "))
    (lena-indent-test-return)
    (should (equal (lena-indent-test-line) ""))
    ;; The blank line in between does not keep its indentation.
    (should (equal (buffer-substring (point-min) (point-max))
                   "theorem test : True := by\n  sorry\n\n"))
    ;; To go on in the block after a blank line, press TAB.
    (lena-indent-test-tab)
    (should (equal (lena-indent-test-line) "  "))))

(ert-deftest lena-indent-test-return-keeps-dedent ()
  "RET does not reindent the line it ends, which may be dedented by hand."
  (lena-indent-test-with-buffer "  · exact a\n· exact b"
    (lena-indent-test-return)
    (forward-line -1)
    (should (equal (lena-indent-test-line) ""))
    (should (looking-at-p "· exact b"))))

(ert-deftest lena-indent-test-tab ()
  "TAB indents to the target, then one level deeper per press."
  (lena-indent-test-with-buffer "theorem foo : True := by\n"
    (lena-indent-test-tab)
    (should (equal (lena-indent-test-line) "  "))
    (lena-indent-test-tab)
    (should (equal (lena-indent-test-line) "    "))
    (setq this-command 'lena-indent-dedent)
    (call-interactively #'lena-indent-dedent)
    (should (equal (lena-indent-test-line) "  "))
    (call-interactively #'lena-indent-dedent)
    (call-interactively #'lena-indent-dedent)
    (should (equal (lena-indent-test-line) ""))))

(ert-deftest lena-indent-test-tab-in-text ()
  "TAB within the text fixes the indentation, keeping point in the text."
  (lena-indent-test-with-buffer "  foo\n     bar"
    (backward-char)
    (lena-indent-test-tab)
    (should (equal (lena-indent-test-line) "  ba")))
  ;; A line that is indented already is left alone, so that TAB may
  ;; complete instead.
  (lena-indent-test-with-buffer "  foo\n  bar"
    (backward-char)
    (lena-indent-test-tab)
    (should (equal (lena-indent-test-line) "  ba"))
    (should (equal (buffer-substring (line-beginning-position)
                                     (line-end-position))
                   "  bar"))))

(ert-deftest lena-indent-test-tab-completes ()
  "TAB completes on an indented line when `tab-always-indent' says so."
  (lena-indent-test-with-buffer "  foo\n  ba"
    (setq-local tab-always-indent 'complete)
    (let ((completion-at-point-functions
           (list (lambda () (list (- (point) 2) (point) '("bar"))))))
      (lena-indent-test-tab))
    (should (equal (lena-indent-test-line) "  bar"))))

(ert-deftest lena-indent-test-offset ()
  "`lena-indent-offset' is the step, and is safe for directory variables."
  (should (safe-local-variable-p 'lena-indent-offset 4))
  (lena-indent-test-with-buffer "theorem foo : True := by"
    (setq-local lena-indent-offset 4)
    (lena-indent-test-return)
    (should (equal (lena-indent-test-line) "    "))))

(ert-deftest lena-indent-test-region ()
  "Reindenting a region leaves it alone."
  (lena-indent-test-with-buffer
      "theorem foo : True := by\n  constructor\n  · exact a\n  · exact b\n\ntheorem bar : True := trivial\n"
    (let ((before (buffer-string)))
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) before)))))

(provide 'lena-indent-tests)

;;; lena-indent-tests.el ends here
