;;; evil-god-toggle-tests.el --- Tests for evil-god-toggle -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jordan Mandel

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests.  Run with:
;;
;;   make test
;;
;; or directly:
;;
;;   emacs -Q --batch -L . -L <evil-dir> -L <god-mode-dir> \
;;         -l evil-god-toggle-tests.el -f ert-run-tests-batch-and-exit
;;
;; Coverage is intentionally modest: pure helpers, state-entry end
;; points, interactive guards, and the rectangle-mark-mode branch of
;; `evil-god-toggle--transition-to-visual' (the fallthrough bug fixed
;; in commit 08706e0).  The timer-driven once-exit paths are not
;; covered here; they fire via `run-at-time' and are better verified
;; interactively.

;;; Code:

(require 'ert)
(require 'evil-god-toggle)

(defmacro evil-god-toggle-tests--with-temp-buffer (&rest body)
  "Run BODY in a fresh temp buffer with Evil enabled and state normal.
Also rebinds the three visual-stash vars to nil for the duration."
  (declare (indent 0))
  `(with-temp-buffer
     (evil-local-mode 1)
     (evil-normal-state)
     (let ((evil-god-toggle--visual-beg nil)
           (evil-god-toggle--visual-end nil)
           (evil-god-toggle--visual-forward nil))
       ,@body)))

;;;; Pure helpers

(ert-deftest evil-god-toggle-test-effective-persist-visual-non-once ()
  "Without ONCE, result is always `evil-god-toggle-persist-visual'."
  (let ((evil-god-toggle-persist-visual 'always)
        (evil-god-toggle-persist-visual-once nil))
    (should (eq (evil-god-toggle--effective-persist-visual nil) 'always)))
  (let ((evil-god-toggle-persist-visual nil)
        (evil-god-toggle-persist-visual-once 'always))
    (should (eq (evil-god-toggle--effective-persist-visual nil) nil))))

(ert-deftest evil-god-toggle-test-effective-persist-visual-once-follow ()
  "With ONCE t and persist-visual-once=`follow', uses persist-visual."
  (let ((evil-god-toggle-persist-visual 'to-god)
        (evil-god-toggle-persist-visual-once 'follow))
    (should (eq (evil-god-toggle--effective-persist-visual t) 'to-god))))

(ert-deftest evil-god-toggle-test-effective-persist-visual-once-explicit ()
  "With ONCE t and a non-`follow' once value, uses the once value."
  (let ((evil-god-toggle-persist-visual 'always)
        (evil-god-toggle-persist-visual-once 'to-evil))
    (should (eq (evil-god-toggle--effective-persist-visual t) 'to-evil))))

(ert-deftest evil-god-toggle-test-determine-target-state-hit ()
  "Previous state mapped in alist returns the mapped value."
  (let ((evil-god-toggle-target-state-alist
         '((insert . normal) (replace . normal))))
    (should (eq (evil-god-toggle--determine-target-state 'insert) 'normal))
    (should (eq (evil-god-toggle--determine-target-state 'replace) 'normal))))

(ert-deftest evil-god-toggle-test-determine-target-state-miss ()
  "Previous state not in alist returns the state itself."
  (let ((evil-god-toggle-target-state-alist '((insert . normal))))
    (should (eq (evil-god-toggle--determine-target-state 'visual) 'visual))))

(ert-deftest evil-god-toggle-test-determine-target-state-empty ()
  "Empty alist returns previous state unchanged."
  (let ((evil-god-toggle-target-state-alist nil))
    (should (eq (evil-god-toggle--determine-target-state 'normal) 'normal))))

;;;; State entries

(ert-deftest evil-god-toggle-test-enter-god ()
  "`-execute-in-god-state' lands in evil god state."
  (evil-god-toggle-tests--with-temp-buffer
    (evil-god-toggle-execute-in-god-state)
    (should (eq evil-state 'god))))

(ert-deftest evil-god-toggle-test-enter-god-off ()
  "`-execute-in-god-off-state' lands in evil god-off state."
  (evil-god-toggle-tests--with-temp-buffer
    (evil-god-toggle-execute-in-god-off-state)
    (should (eq evil-state 'god-off))))

(ert-deftest evil-god-toggle-test-bail-from-god ()
  "Bail from god returns to Evil normal."
  (evil-god-toggle-tests--with-temp-buffer
    (evil-god-toggle-execute-in-god-state)
    (evil-god-toggle-bail)
    (should (eq evil-state 'normal))))

(ert-deftest evil-god-toggle-test-toggle-into-god-and-back ()
  "`evil-god-toggle-god-toggle' flips between god and god-off."
  (evil-god-toggle-tests--with-temp-buffer
    (evil-god-toggle-god-toggle)
    (should (eq evil-state 'god))
    (evil-god-toggle-god-toggle)
    (should (eq evil-state 'god-off))))

;;;; Guards

(ert-deftest evil-god-toggle-test-once-in-god-errors ()
  "`evil-god-toggle-once' from god state signals user-error."
  (evil-god-toggle-tests--with-temp-buffer
    (evil-god-toggle-execute-in-god-state)
    (should-error (evil-god-toggle-once) :type 'user-error)))

(ert-deftest evil-god-toggle-test-off-once-in-god-off-errors ()
  "`evil-god-toggle-off-once' from god-off state signals user-error."
  (evil-god-toggle-tests--with-temp-buffer
    (evil-god-toggle-execute-in-god-off-state)
    (should-error (evil-god-toggle-off-once) :type 'user-error)))

;;;; --transition-to-visual

(ert-deftest evil-god-toggle-test-transition-to-visual-with-stash ()
  "A stashed beg/end restores a charwise visual selection and clears the stash."
  (evil-god-toggle-tests--with-temp-buffer
    (insert "hello world")
    (goto-char (point-min))
    (setq evil-god-toggle--visual-beg 1
          evil-god-toggle--visual-end 4
          evil-god-toggle--visual-forward t)
    (evil-god-toggle--transition-to-visual)
    (should (eq evil-state 'visual))
    (should (null evil-god-toggle--visual-beg))
    (should (null evil-god-toggle--visual-end))
    (should (null evil-god-toggle--visual-forward))))

(ert-deftest evil-god-toggle-test-transition-to-visual-no-stash-fallback ()
  "Without a stash, falls back to `evil-visual-char'."
  (evil-god-toggle-tests--with-temp-buffer
    (insert "abc")
    (goto-char (point-min))
    (evil-god-toggle--transition-to-visual)
    (should (eq evil-state 'visual))))

(ert-deftest evil-god-toggle-test-transition-to-visual-rectangle ()
  "In rectangle-mark-mode: clear stash, deactivate mark, do NOT enter visual.
Regression test for the fallthrough bug where the rectangle branch
scheduled normal-state via a timer and then continued past the `cond'
to enter charwise visual synchronously."
  (evil-god-toggle-tests--with-temp-buffer
    (insert "abc")
    (goto-char (point-min))
    (push-mark (point) t t)
    (goto-char (+ (point-min) 2))
    (rectangle-mark-mode 1)
    (setq evil-god-toggle--visual-beg 1
          evil-god-toggle--visual-end 2
          evil-god-toggle--visual-forward t)
    (evil-god-toggle--transition-to-visual)
    (should (null evil-god-toggle--visual-beg))
    (should (null evil-god-toggle--visual-end))
    (should (null evil-god-toggle--visual-forward))
    (should-not (region-active-p))
    (should-not (eq evil-state 'visual))))

;;;; Once lifecycle (pending-flag skip)

(ert-deftest evil-god-toggle-test-exit-once-skips-triggering-command ()
  "`--exit-once' running under the triggering command's post-command-hook
must not exit immediately.  Regression test for a1b2c3d: a prior
refactor made the skip depend on `this-command' matching
`evil-god-toggle-once', which broke bindings that wrap the command
in a lambda (where `this-command' is the lambda, not the command)."
  (evil-god-toggle-tests--with-temp-buffer
    ;; Simulate what entry does: schedule the exit hook with pending=t.
    (evil-god-once-state)
    ;; Entry hook already ran via evil-define-state; verify pending=t.
    (should evil-god-toggle--exit-once-pending)
    (should (eq evil-state 'god-once))
    ;; First firing (the "triggering command" post-command) — simulate
    ;; this-command being something other than evil-god-toggle-once.
    (let ((this-command (lambda () (interactive))))
      (evil-god-toggle--exit-once))
    ;; Still in god-once, pending flag consumed.
    (should (eq evil-state 'god-once))
    (should-not evil-god-toggle--exit-once-pending)))

(ert-deftest evil-god-toggle-test-exit-once-skips-prefix-arg ()
  "`--exit-once' does not fire while the user is building a prefix arg."
  (evil-god-toggle-tests--with-temp-buffer
    (evil-god-once-state)
    (setq evil-god-toggle--exit-once-pending nil) ; consume entry's skip
    (let ((prefix-arg '(4)))
      (evil-god-toggle--exit-once))
    (should (eq evil-state 'god-once))))

(ert-deftest evil-god-toggle-test-exit-once-removes-hook-outside-god-once ()
  "`--exit-once' self-removes from `post-command-hook' when state leaves
god-once by any other path."
  (evil-god-toggle-tests--with-temp-buffer
    (evil-god-once-state)
    (add-hook 'post-command-hook #'evil-god-toggle--exit-once nil t)
    (evil-normal-state)
    (evil-god-toggle--exit-once)
    (should-not (memq #'evil-god-toggle--exit-once post-command-hook))))

;;;; Buffer-locality

(ert-deftest evil-god-toggle-test-last-command-buffer-local ()
  "`evil-god-toggle--last-command' is defined with `defvar-local'."
  (should (local-variable-if-set-p 'evil-god-toggle--last-command)))

(provide 'evil-god-toggle-tests)
;;; evil-god-toggle-tests.el ends here
