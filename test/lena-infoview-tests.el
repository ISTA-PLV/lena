;;; lena-infoview-tests.el --- Tests for lena-infoview  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see lena/LICENSE.GPL3.

;;; Commentary:

;; Tests for the infoview buffer of `lena-infoview' that need no Lean
;; server: recorded goals are shown directly.

;;; Code:

(require 'ert)
(require 'lena-infoview)
(require 'lena-test-helpers)

(ert-deftest lena-infoview-test-show ()
  (lena-test-with-goals
    (should (string-prefix-p "Tactic state: 3 goals\ngoal 1\n"
                             (buffer-string)))
    (should (= (seq-count (lambda (overlay)
                            (and (overlay-get overlay 'mouse-face)
                        (not (overlay-get overlay 'outline-button))))
                          (overlays-in (point-min) (point-max)))
               63))
    (should (string-search "SpinLock.lean:110:2"
                           (lena-infoview--mode-line-process)))
    (should-not header-line-format)))

(ert-deftest lena-infoview-test-own-overlays ()
  "Rendering again replaces the infoview's overlays, and only those."
  (lena-test-with-goals
    (cl-flet ((count (property)
                ;; Since Emacs 31, Outline makes its buttons on redisplay.
                (when jit-lock-mode (jit-lock-fontify-now))
                (seq-count (lambda (overlay) (overlay-get overlay property))
                           (overlays-in (point-min) (point-max)))))
      (let ((foreign (make-overlay 1 5))
            (nodes (count 'lena-infoview-node))
            (buttons (count 'outline-button)))
        (should (> nodes 0))
        (should (> buttons 0))
        (lena-test-goto "IProp GF")
        (lena-infoview--highlight-point)
        (lena-infoview-toggle-types)
        (lena-infoview-toggle-types)
        (should (overlay-buffer foreign))
        (should (= (count 'lena-infoview-node) nodes))
        (should (= (count 'outline-button) buttons))
        (should (= (seq-count (lambda (overlay)
                                (eq (overlay-get overlay 'face)
                           'lena-infoview-point-node))
                              (overlays-in (point-min) (point-max)))
                   1))))))

(ert-deftest lena-infoview-test-dim ()
  "Outdated content is dimmed, code included, even when rendered again."
  (lena-test-with-goals
    (cl-flet ((dimmed-p (pos)
                (memq 'shadow (ensure-list (get-char-property pos 'face)))))
      (let ((code (save-excursion
                    (lena-test-goto "IProp GF")
                    (prop-match-beginning
                     (text-property-search-forward 'face)))))
        (should (get-text-property code 'face))
        (should-not (dimmed-p code))
        (lena-infoview--set-status 'processing)
        (should (dimmed-p code))
        (should (dimmed-p (1- (point-max))))
        (lena-infoview-toggle-types)
        (should (dimmed-p (1- (point-max))))
        (lena-infoview-toggle-types)
        (lena-infoview--set-status nil)
        (should-not (dimmed-p code))))))

(ert-deftest lena-infoview-test-wrap ()
  "Long lines wrap in the infoview, even where Emacs truncates by default."
  (let ((default (default-value 'truncate-lines)))
    (setq-default truncate-lines t)
    (unwind-protect
        (lena-test-with-goals
          (should-not truncate-lines)
          (should word-wrap)
          (should (get-text-property (point-min) 'wrap-prefix))
          (with-current-buffer (lena-infoview--help-buffer)
            (unwind-protect
                (progn (should-not truncate-lines)
                       (should word-wrap))
              (kill-buffer))))
      (setq-default truncate-lines default))))

(defun lena-infoview-test-button ()
  "Return the icon of the outline button of the heading at point, or nil.
In the margin, the button is a space displaying the icon there."
  ;; Since Emacs 31, Outline makes its buttons on redisplay.
  (when jit-lock-mode (jit-lock-fontify-now))
  (when-let* ((overlay (seq-find (lambda (o) (overlay-get o 'outline-button))
                                 (overlays-at (line-beginning-position))))
              (string (overlay-get overlay 'before-string)))
    (pcase (get-text-property 0 'display string)
      (`((margin left-margin) ,icon) icon))))

(ert-deftest lena-infoview-test-fold-buttons ()
  "Foldable headings have buttons, in the margin."
  (lena-test-with-goals
    (should (= left-margin-width 2))
    (lena-test-goto "goal 1")
    (should (lena-infoview-test-button))
    (lena-test-goto "hlc : HasLC")
    (should-not (lena-infoview-test-button))))

(defun lena-infoview-test-renumber (goals suffix)
  "Return GOALS with fresh metavariable ids ending in SUFFIX.
That is what the goals look like after the next tactic."
  (list :goals (vconcat
                (mapcar (lambda (goal)
                          (plist-put (copy-sequence goal) :mvarId
                                     (concat (plist-get goal :mvarId) suffix)))
                        (plist-get goals :goals)))))

(ert-deftest lena-infoview-test-fold-survives-tactic ()
  "A folded goal stays folded at the same place after the next tactic."
  (lena-test-with-goals
    (lena-test-goto "goal 2")
    (outline-hide-subtree)
    (lena-infoview--show (lena-infoview-test-renumber
                          (lena-test-spinlock-goals) ".next")
                         nil (lena-rpc-make-owner) "SpinLock.lean:111:2")
    (lena-test-goto "goal 2")
    (should (outline-invisible-p (line-end-position)))
    (lena-test-goto "goal 1")
    (should-not (outline-invisible-p (line-end-position)))))

(ert-deftest lena-infoview-test-fold-case-by-name ()
  "A folded case stays folded where it moves, an unfolded one open."
  (lena-test-with-options
   (let ((inl (lena-test-goal :userName "inl"))
        (inr (lena-test-goal :userName "inr")))
    (unwind-protect
        (progn
          (lena-infoview--show (list :goals (vector inl inr)) nil
                               (lena-rpc-make-owner) "x")
          (with-current-buffer (lena-infoview--buffer)
            (lena-test-goto "case inr")
            (outline-hide-subtree)
            ;; `inl' is solved; `inr' is now the first goal.
            (lena-infoview--show (list :goals (vector inr)) nil
                                 (lena-rpc-make-owner) "x")
            (lena-test-goto "case inr")
            (should (outline-invisible-p (line-end-position)))
            (lena-infoview--show (list :goals (vector inl inr)) nil
                                 (lena-rpc-make-owner) "x")
            (lena-test-goto "case inl")
            (should-not (outline-invisible-p (line-end-position)))))
      (kill-buffer (lena-infoview--buffer))))))

(ert-deftest lena-infoview-test-filters ()
  (lena-test-with-goals
    (should (string-search "inst✝" (buffer-string)))
    (lena-infoview-toggle-instances)
    (should-not (string-search "inst✝" (buffer-string)))
    (should lena-infoview-hide-instances)
    (lena-infoview-toggle-instances)
    (should (string-search "inst✝" (buffer-string)))))

(ert-deftest lena-infoview-test-view-options ()
  "A menu shows and changes the view options."
  (lena-test-with-goals
    (should (eq (key-binding "v") #'lena-infoview-view-options))
    (should (transient-get-suffix 'lena-infoview-view-options "i"))
    (should (equal (lena-infoview--switch-description "Hide types" t)
                   "Hide types on"))
    (cl-flet ((current ()
                (let ((description (lena-infoview--expected-type-description)))
                  (seq-find (lambda (value)
                              (let ((i (string-search (symbol-name value)
                                             description)))
                                (eq (get-text-property i 'face description)
                           'transient-value)))
                            '(expanded collapsed hidden)))))
      (should (eq (current) 'expanded))
      (lena-infoview-cycle-expected-type)
      (should (eq lena-infoview-expected-type 'collapsed))
      (should (eq (current) 'collapsed))
      (lena-infoview-cycle-expected-type)
      (should (eq (current) 'hidden))
      (lena-infoview-cycle-expected-type)
      (should (eq (current) 'expanded)))))

(ert-deftest lena-infoview-test-toggle-diffs ()
  "Diffs can be switched off and on."
  (lena-test-with-goals
    (cl-flet ((marked-p ()
                (text-property-search-forward
                 'face 'lena-render-diff-removed
                 (lambda (face faces) (memq face (ensure-list faces))))))
      (goto-char (point-min))
      (should (marked-p))
      (lena-infoview-toggle-diffs)
      (goto-char (point-min))
      (should-not (marked-p))
      (lena-infoview-toggle-diffs)
      (goto-char (point-min))
      (should (marked-p)))))

(ert-deftest lena-infoview-test-expected-type-collapsed ()
  "A collapsed expected type starts folded and stays as the user leaves it."
  (lena-test-with-options
    (let ((lena-infoview-expected-type 'collapsed)
          (term-goal (list :hyps [] :type (lena-test-code "Nat"))))
      (unwind-protect
          (progn
            (lena-infoview--show nil term-goal (lena-rpc-make-owner) "x")
            (with-current-buffer (lena-infoview--buffer)
              (lena-test-goto "Expected type")
              (should (outline-invisible-p (line-end-position)))
              (outline-show-subtree)
              (lena-infoview--show nil term-goal (lena-rpc-make-owner) "x")
              (lena-test-goto "Expected type")
              (should-not (outline-invisible-p (line-end-position)))
              ;; Gone and back: folded again.
              (lena-infoview--show nil nil (lena-rpc-make-owner) "x")
              (lena-infoview--show nil term-goal (lena-rpc-make-owner) "x")
              (lena-test-goto "Expected type")
              (should (outline-invisible-p (line-end-position)))))
        (kill-buffer (lena-infoview--buffer))))))

(ert-deftest lena-infoview-test-goal-navigation ()
  (lena-test-with-goals
    (lena-infoview-next-goal)
    (should (looking-at "goal 1"))
    (lena-infoview-next-goal)
    (should (looking-at "goal 2"))
    (forward-line 2)
    (lena-infoview-previous-goal)
    (should (looking-at "goal 2"))
    (lena-infoview-previous-goal)
    (should (looking-at "goal 1"))
    (should-error (lena-infoview-previous-goal) :type 'user-error)
    (lena-infoview-next-goal 2)
    (should (looking-at "goal 3"))
    (should-error (lena-infoview-next-goal) :type 'user-error)))

(ert-deftest lena-infoview-test-copy-goal ()
  (lena-test-with-goals
    (lena-test-goto "goal 3")
    (forward-line 3)
    (let ((kill-ring nil))
      (lena-infoview-copy-goal)
      (should (string-prefix-p "goal 3\nhlc : HasLC\n" (car kill-ring)))
      (should (string-suffix-p "⊢ GName" (car kill-ring)))
      (should-not (text-properties-at 0 (car kill-ring))))))

(ert-deftest lena-infoview-test-point-highlight ()
  "The innermost subterm at point is highlighted, and only that."
  (lena-test-with-goals
    (lena-test-goto "IProp GF")
    (search-forward "G")
    (lena-infoview--highlight-point)
    (let ((overlay lena-infoview--point-overlay)
          (node (lena-render-node-at (point))))
      (should (equal (buffer-substring-no-properties
                      (overlay-start overlay) (overlay-end overlay))
                     "GF"))
      ;; Buffer positions are the renderer's positions.
      (should (= (overlay-start overlay) (lena-render-node-beg node))))
    (goto-char (point-min))
    (lena-infoview--highlight-point)
    (should-not lena-infoview--point-overlay)))

(ert-deftest lena-infoview-test-point-highlight-selected ()
  "The subterm at point is highlighted only while the infoview is selected."
  (lena-test-with-windows
    (let ((infoview lena-infoview--window))
      (lena-test-goto "IProp GF")
      (lena-infoview--highlight-point)
      (should lena-infoview--point-overlay)
      ;; Another window is selected; the hook runs in its buffer.
      (select-window (get-buffer-window "Main"))
      (lena-infoview--highlight-point infoview)
      (should-not (buffer-local-value 'lena-infoview--point-overlay
                                      (window-buffer infoview)))
      (select-window infoview)
      (with-current-buffer "Main"
        (lena-infoview--highlight-point infoview))
      (should lena-infoview--point-overlay))))

(ert-deftest lena-infoview-test-window-anchor ()
  "The window keeps its place in a goal when the goals are rendered again."
  (lena-test-with-goals
    (lena-test-goto "goal 3")
    (forward-line 2)
    (forward-char 3)
    (let ((line (buffer-substring-no-properties (line-beginning-position)
                                       (point))))
      ;; Hiding instances removes lines above, in goals 1 to 3.
      (lena-infoview-toggle-instances)
      (should (equal (buffer-substring-no-properties
                      (line-beginning-position) (point))
                     line))
      (should (equal (get-text-property (point) 'lena-goal)
                     (save-excursion (lena-test-goto "goal 3")
                            (get-text-property (point)
                                               'lena-goal)))))))

;;;; Updates:

(defvar lena-infoview-test-calls nil
  "Requests of the update under test, newest first, as (WHAT SUCCESS OWNER).")

(defmacro lena-infoview-test-with-pending-update (&rest body)
  "Run BODY after an update of a source buffer was requested, unanswered.
The requests are in `lena-infoview-test-calls'; the variable `source'
is the source buffer."
  (declare (indent 0) (debug t))
  `(lena-test-with-options
     (let ((lena-infoview-test-calls nil)
           (lena-infoview--pending-owner nil)
           (source (generate-new-buffer "lena-infoview-test-source")))
       (cl-flet ((record (what)
                   (lambda (&rest args)
                     (let* ((success (seq-find #'functionp args))
                            (keys (cdr (memq success args))))
                       (push (list what success (plist-get keys :owner)
                                   (plist-get keys :failure))
                             lena-infoview-test-calls)))))
         (cl-letf (((symbol-function 'lena-rpc-open) (lambda (&rest _) 'handle))
                   ((symbol-function 'lena-rpc-get-interactive-goals)
                    (record 'goals))
                   ((symbol-function 'lena-rpc-get-interactive-term-goal)
                    (record 'term-goal))
                   ((symbol-function 'lena-rpc-get-interactive-diagnostics)
                    (record 'messages))
                   ((symbol-function 'lena-infoview--accomplished-title)
                    #'ignore))
           (unwind-protect
               (progn
                 (with-current-buffer source (insert "a\nb\nc\n"))
                 (lena-infoview--get-buffer-create)
                 (lena-infoview--request source 1)
                 ,@body)
             (kill-buffer source)
             (when-let* ((buffer (lena-infoview--buffer)))
               (kill-buffer buffer))))))))

(defun lena-infoview-test-answer (&optional failing)
  "Answer the requests of the update under test: one goal, nothing else.
The request for FAILING, one of `goals', `term-goal' and `messages',
fails instead."
  (dolist (call lena-infoview-test-calls)
    (if (eq (car call) failing)
        (funcall (nth 3 call) '(:code -32603 :message "Broken"))
      (funcall (nth 1 call)
               (and (eq (car call) 'goals)
                    (list :goals (vector (lena-test-goal))))))))

(ert-deftest lena-infoview-test-update-answered ()
  "The answers to an update are shown together."
  (lena-infoview-test-with-pending-update
    (should (equal (sort (mapcar #'car lena-infoview-test-calls) #'string<)
                   '(goals messages term-goal)))
    (lena-infoview-test-answer)
    (with-current-buffer (lena-infoview--buffer)
      (should (string-search "⊢ n = 0" (buffer-string)))
      (should-not lena-infoview--status))))

(ert-deftest lena-infoview-test-update-failed ()
  "A failed request for goals is reported; one for messages only drops them."
  (lena-infoview-test-with-pending-update
    (let ((owner (nth 2 (car lena-infoview-test-calls))))
      (lena-infoview-test-answer 'goals)
      (should (lena-rpc-owner-released owner))
      (with-current-buffer (lena-infoview--buffer)
        (should (equal lena-infoview--status '(error . "Broken")))
        (should-not (string-search "⊢ n = 0" (buffer-string))))))
  (lena-infoview-test-with-pending-update
    (lena-infoview-test-answer 'messages)
    (with-current-buffer (lena-infoview--buffer)
      (should-not lena-infoview--status)
      (should (string-search "⊢ n = 0" (buffer-string))))))

(ert-deftest lena-infoview-test-update-after-kill ()
  "Answers that arrive after the infoview was killed are dropped."
  (lena-infoview-test-with-pending-update
    (let ((owner (nth 2 (car lena-infoview-test-calls))))
      (kill-buffer (lena-infoview--buffer))
      (should (lena-rpc-owner-released owner))
      (lena-infoview-test-answer)
      ;; Not shown in a buffer made anew.
      (should-not (lena-infoview--buffer)))))

(ert-deftest lena-infoview-test-update-superseded-by-processing ()
  "Moving to a line being processed drops the answers for the old one."
  (lena-infoview-test-with-pending-update
    (let ((owner (nth 2 (car lena-infoview-test-calls)))
          (lena-infoview--target (cons source 5)))
      (cl-letf (((symbol-function 'lena-infoview-visible-p) #'always)
                ((symbol-function 'eglot-managed-p) #'always)
                ((symbol-function 'lena-infoview--rpc-supported-p) #'always)
                ((symbol-function 'lena-eglot-processing-line-p) #'always))
        (lena-infoview--update))
      (should (lena-rpc-owner-released owner))
      (lena-infoview-test-answer)
      (with-current-buffer (lena-infoview--buffer)
        (should (eq lena-infoview--status 'processing))
        (should-not lena-infoview--results)
        (should lena-infoview--dim-overlay)))))

(defun lena-infoview-test-file-diagnostic (message &rest keys)
  "Return an LSP diagnostic with MESSAGE about the whole of a 3-line file.
KEYS are added to it."
  (append (list :range '(:start (:line 0 :character 0)
                                :end (:line 1 :character 0))
                :fullRange '(:start (:line 0 :character 0)
                                    :end (:line 3 :character 0))
                :severity 3 :message message)
          keys))

(defun lena-infoview-test-update-processing (source diagnostics)
  "Update the infoview while the server processes SOURCE with DIAGNOSTICS.
An update for line 2 was pending; return the owner of its references."
  (let ((owner (nth 2 (car lena-infoview-test-calls)))
        (lena-infoview--target (cons source 5)))
    (with-current-buffer source
      (setq-local lena-eglot-diagnostics diagnostics))
    (setq lena-infoview-test-calls nil)
    (cl-letf (((symbol-function 'lena-infoview-visible-p) #'always)
              ((symbol-function 'eglot-managed-p) #'always)
              ((symbol-function 'lena-infoview--rpc-supported-p) #'always)
              ((symbol-function 'lena-eglot-processing-line-p) #'always))
      (lena-infoview--update))
    owner))

(ert-deftest lena-infoview-test-processing-file-messages ()
  "While the server processes, the messages about the whole file are shown.
They are Lake's output while it builds the imports, undimmed and without
goals; messages about lines and silent ones are left out."
  (lena-infoview-test-with-pending-update
    (let ((owner (lena-infoview-test-update-processing
                  source
                  (vector
                   (lena-infoview-test-file-diagnostic
                    "✔ [3/12] Built Speclang.Basic (1.2s)")
                   (lena-infoview-test-file-diagnostic "Silent" :isSilent t)
                   (list :range '(:start (:line 1 :character 0)
                                         :end (:line 1 :character 1))
                         :severity 1 :message "On a line")))))
      (should (lena-rpc-owner-released owner))
      (should-not lena-infoview-test-calls)
      (with-current-buffer (lena-infoview--buffer)
        (should (eq lena-infoview--status 'processing))
        (should-not lena-infoview--dim-overlay)
        (should (string-search "Messages (1)" (buffer-string)))
        (should (string-search "✔ [3/12] Built Speclang.Basic (1.2s)"
                               (buffer-string)))
        (should-not (string-search "Silent" (buffer-string)))
        (should-not (string-search "On a line" (buffer-string))))
      ;; Once Lake is done, Lean clears its output and loads the
      ;; imports: the output stays, dimmed.
      (lena-infoview-test-update-processing source [])
      (with-current-buffer (lena-infoview--buffer)
        (should (eq lena-infoview--status 'processing))
        (should lena-infoview--dim-overlay)
        (should (string-search "✔ [3/12] Built Speclang.Basic (1.2s)"
                               (buffer-string)))))))

(ert-deftest lena-infoview-test-processing-imports-out-of-date ()
  "A message that the imports are out of date keeps its restart button."
  (lena-infoview-test-with-pending-update
    (lena-infoview-test-update-processing
     source
     (vector (lena-infoview-test-file-diagnostic
              "Imports are out of date and should be rebuilt; use the \
\"Restart File\" command in your editor.")))
    (with-current-buffer (lena-infoview--buffer)
      (should (string-search "[restart file]" (buffer-string)))
      (goto-char (point-min))
      (should (text-property-search-forward 'lena-restart-file)))))

;;;; Windows:

(ert-deftest lena-infoview-test-jump-out ()
  "A jump from the infoview shows its target in another window."
  (lena-test-with-windows
    (let ((target (get-buffer-create "Target")))
      (unwind-protect
          (progn
            ;; As `xref-find-definitions' does.
            (switch-to-buffer target)
            (should (eq (window-buffer lena-infoview--window)
                        (lena-infoview--buffer)))
            (should (eq (window-buffer) target))
            (should-not (eq (selected-window) lena-infoview--window)))
        (kill-buffer target)))))

(ert-deftest lena-infoview-test-jump-back ()
  "Going back to the infoview selects its window, not a second one."
  (lena-test-with-windows
    (let ((home lena-infoview--window)
          (pos (save-excursion (lena-test-goto "goal 2") (point))))
      (select-window (get-buffer-window "Main"))
      ;; As `xref-go-back' does, then the command loop.
      (switch-to-buffer (lena-infoview--buffer))
      (goto-char pos)
      (lena-infoview--keep-window)
      (should (eq (selected-window) home))
      (should (= (point) pos))
      (should (equal (sort (mapcar #'buffer-name
                          (mapcar #'window-buffer (window-list)))
                           #'string<)
                     (list "*Lena Infoview*" "Main")))
      ;; Nothing happens in the infoview's own window.
      (lena-infoview--keep-window)
      (should (eq (selected-window) home)))))

;;;; Complete proofs:

(ert-deftest lena-infoview-test-accomplished-title ()
  "Above goals in a complete proof, a line says so; it replaces no goals."
  (lena-test-with-options
    (unwind-protect
        (progn
          (lena-infoview--show (lena-test-spinlock-goals) nil
                               (lena-rpc-make-owner) "SpinLock.lean:110:2"
                               nil "Goals accomplished 🎉")
          (with-current-buffer (lena-infoview--buffer)
            (should (string-prefix-p
                     "Goals accomplished 🎉\nTactic state: 3 goals\ngoal 1\n"
                     (buffer-string)))
            ;; A line of its own, not a heading that folds the goals.
            (goto-char (point-min))
            (should-not (outline-on-heading-p))
            (should (eq (get-text-property (point) 'face)
                        'lena-render-goals-accomplished))
            (forward-line)
            (should (outline-on-heading-p)))
          ;; No goals left: the line instead of "no goals".
          (lena-infoview--show (list :goals []) nil (lena-rpc-make-owner)
                               "A.lean:2:6" nil "Goals accomplished 🎉")
          (with-current-buffer (lena-infoview--buffer)
            (should (equal (string-trim (buffer-string))
                           "Goals accomplished 🎉")))
          (lena-infoview--show (list :goals []) nil (lena-rpc-make-owner)
                               "A.lean:2:6")
          (with-current-buffer (lena-infoview--buffer)
            (should (string-prefix-p "Tactic state: no goals"
                            (buffer-string))))
          ;; Without goals, the title alone.
          (lena-infoview--show nil nil (lena-rpc-make-owner) "A.lean:1:0"
                               nil "Goals accomplished 🎉")
          (with-current-buffer (lena-infoview--buffer)
            (should (equal (string-trim (buffer-string))
                           "Goals accomplished 🎉")))
          (lena-infoview--show nil nil (lena-rpc-make-owner) "A.lean:1:0")
          (with-current-buffer (lena-infoview--buffer)
            (should (string-prefix-p "No goals" (buffer-string)))))
      (when-let* ((buffer (lena-infoview--buffer)))
        (kill-buffer buffer)))))

(defmacro lena-infoview-test-with-proof (&rest body)
  "Run BODY in a buffer of 10 lines whose lines 2 to 5 are a complete proof."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (dotimes (i 10) (insert (format "line %d\n" i)))
     (setq lena-eglot-diagnostics
           (vector (lena-test-diagnostic 3 '(2 . 0) '(5 . 6)
                                         :tags '(2) :silent t)))
     ,@body))

(defun lena-infoview-test-line (line)
  "Return the position of the start of the 0-based LINE."
  (save-excursion (goto-char (point-min)) (forward-line line) (point)))

(ert-deftest lena-infoview-test-accomplished ()
  "The title comes from the diagnostics and the sign of the source buffer."
  (lena-infoview-test-with-proof
    (let ((lena-margin-goals-accomplished "🎉"))
      (cl-letf (((symbol-function 'char-displayable-p) #'always))
        (cl-flet ((title (line)
                    (lena-infoview--accomplished-title
                     (current-buffer) (lena-infoview-test-line line))))
          (should-not (title 1))
          (should (equal (title 3) "Goals accomplished 🎉"))
          (setq-local lena-margin-goals-accomplished "🐣")
          (should (equal (title 5) "Goals accomplished 🐣"))
          (setq-local lena-margin-goals-accomplished "")
          (should (equal (title 2) "Goals accomplished")))))))

(ert-deftest lena-infoview-test-accomplished-trigger ()
  "The infoview updates when the diagnostics around point change."
  (lena-infoview-test-with-proof
    (let ((scheduled 0)
          (diagnostics lena-eglot-diagnostics)
          (lena-infoview--target (cons (current-buffer)
                                       (lena-infoview-test-line 4))))
      (cl-letf (((symbol-function 'lena-infoview-visible-p) #'always)
                ((symbol-function 'lena-infoview--schedule)
                 (lambda (&rest _) (cl-incf scheduled))))
        (setq lena-infoview--diagnostics-key nil)
        ;; The range of the complete proof is only its first line.
        (lena-infoview--diagnostics diagnostics)
        (should (= scheduled 1))
        (lena-infoview--diagnostics diagnostics)
        (should (= scheduled 1))
        (lena-infoview--diagnostics [])
        (should (= scheduled 2))))))

(provide 'lena-infoview-tests)

;;; lena-infoview-tests.el ends here
