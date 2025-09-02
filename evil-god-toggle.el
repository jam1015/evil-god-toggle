;;; evil-god-toggle.el --- Toggle Evil and God Mode -*- lexical-binding: t -*-
;; Version: 0.1.0
;;
;; Copyright (C) 2025 Jordan Mandel
;; Author: Jordan Mandel <jordan.mandel@live.com>
;; Created: 2025-04-22
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (evil "1.0.8") (god-mode "2.12.0"))
;; Keywords: convenience, emulation, evil, god-mode
;; Homepage: https://github.com/jam1015/evil-god-toggle
;;
;; This file is not part of GNU Emacs.
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;; This package toggles between Evil mode and God mode, enabling users
;; to switch seamlessly.
;; Provided states:
;; - `god': Full persistent God mode.
;; - `god-once': Enter God mode for one command, then return to Evil.
;; - `god-off': Like evil's built-in Emacs state.  Easy to switch to and from.
;; Ideal for users blending Emacs and Vim-style modal editing.
;;
;;; Code:

(declare-function evil-change-to-previous-state "evil")
(declare-function evil-visual-activate-hook "evil")
(declare-function evil-visual-deactivate-hook "evil")
(declare-function universal-argument-minus "simple")
(declare-function universal-argument-other-key "simple")
(require 'evil)
(require 'god-mode)

;; Customization group

;;;###autoload
(defgroup evil-god-toggle nil
  "Customization group for evil-god-toggle."
  :group 'convenience
  :prefix "evil-god-toggle-")


;; customization options

;;;###autoload
(defcustom evil-god-toggle-persist-visual 'always
  "Control persistence of visual selection when toggling."
  :type '(choice (const :tag "Always" always)
                 (const :tag "To God" to-god)
                 (const :tag "To Evil" to-evil)
                 (const :tag "Never" nil))
  :group 'evil-god-toggle)

;;;###autoload
(defcustom evil-god-toggle-global nil
  "If non-nil, use `god-mode-all` instead of `god-local-mode` (per-buffer)."
  :type 'boolean
  :group 'evil-god-toggle)


(defvar evil-god-toggle--once-buffer nil
  "The buffer in which `god-once` was entered, for restoring Evil state.")

(defvar evil-god-toggle-mode-map (make-sparse-keymap)
  "Keymap for `evil-god-toggle-mode'.")


(defvar-local evil-god-toggle--visual-hooks-removed nil
  "Non-nil if evil-god-toggle removed visual mark hooks in this buffer.")

(defvar-local evil-god-toggle--has-fix-last-command-hook nil
  "Non-nil if `evil-god-toggle--fix-last-command` in local `pre-command-hook`.")

(defvar-local evil-god-toggle--has-exit-once-hook nil
  "Non-nil if `evil-god-toggle--exit-once` in local `post-command-hook`.")

(defvar evil-god-toggle--last-command nil
  "Command executed just before entering god state.")

(defvar evil-god-toggle--visual-forward nil
  "Non-nil if the saved region was selected forward (mark ≤ point).")
(defvar-local evil-god-toggle--visual-beg nil
  "Buffer position of region start to restore when toggling back to Evil visual.")
(defvar-local evil-god-toggle--visual-end nil
  "Buffer position of region end to restore when toggling back to Evil visual.")

;;;###autoload
(define-minor-mode evil-god-toggle-mode
  "Global mode for Evil/God toggling.
Creates `god-mode' states for Evil."
  :global t
  :group 'evil-god-toggle
  :lighter " EGT"
  :keymap evil-god-toggle-mode-map
 (if evil-god-toggle-mode
      ;; Mode is being turned ON
      (add-hook 'evil-visual-state-entry-hook
                #'evil-god-toggle--check-and-update-previous-state-visual)
    ;; Mode is being turned OFF
    (remove-hook 'evil-visual-state-entry-hook
                 #'evil-god-toggle--check-and-update-previous-state-visual)))

;;;###autoload
(defun evil-god-toggle-god-toggle ()
  "Toggle between God mode and Evil mode.
Optional helper function."
  (interactive)
  (if (minibufferp)
      (user-error "Cannot enter God mode from minibuffer")
    (cond
     ;; Handle toggling from God mode to another Evil state
     ((eq evil-state 'god)
     (evil-god-toggle-execute-in-god-off-state))
     ;;;; Default case for any other states
     (t (evil-god-toggle-execute-in-god-state)))))

;;;###autoload
(defun evil-god-toggle-execute-in-god-off-state ()
  "Exit God mode (force `god-off'), optionally restoring visual selection.
If called from the minibuffer, signal a user-error."
  (interactive)
  (if (minibufferp)
      (user-error "Cannot enter `god-off' mode from minibuffer")
    ;; original behavior
    (evil-god-toggle--add-fix-last)
    (setq evil-god-toggle--last-command last-command)
    (evil-god-toggle--maybe-restore-visual 'evil-god-off-state)))

;;;###autoload
(defun evil-god-toggle-execute-in-god-state ()
  "Go into God state, preserving visual selection if configured."
  (interactive)
  (setq evil-god-toggle--last-command last-command)
  (evil-god-toggle--add-fix-last)
  (evil-god-toggle--maybe-restore-visual  'evil-god-state))

;;;###autoload
(defun evil-god-toggle-stop-execute-in-god-state (target)
  "Wrapper for leaving god state and switching to TARGET evil state.
TARGET should be a symbol: `normal', `insert', or `visual'.
Does not respect `evil-god-toggle-visual-persist'"
  (interactive)
  (pcase target
    ('normal (evil-god-toggle--transition-to-normal))
    ('insert (evil-god-toggle--transition-to-insert))
    ('visual (evil-god-toggle--transition-to-visual))
    (_       (evil-god-toggle--transition-to-normal))) ;; fallback
  (force-mode-line-update))

;;;###autoload
(defun evil-god-toggle-stop-choose-state (alternate_target)
  "From God, toggle back into Evil, choosing appropriate state.
Restore visual or going to state specified by `ALTERNATE_TARGET'.
Alternate_target can be \='normal \='insert or \='visual .
If there's an active region AND either persist-visual flag is t,
stash its bounds **and** direction, then call visual; else normal.
This is the function that should be used for keybindings because
it it respects `evil-god-toggle-visual-persist'"
  (interactive)
  ;; only if a region is selected
  (if (and (use-region-p)  (memq evil-god-toggle-persist-visual '(always to-evil)))
      (let* ((m       (mark))
             (p       (point))
             (beg     (min m p))
             ;; Emacs's region-end is exclusive, so -1 to make it inclusive
             (end     (1- (max m p)))
             ;; mark before point?
             (forward (<= m p)))
        (setq evil-god-toggle--visual-beg     beg
              evil-god-toggle--visual-end     end
              evil-god-toggle--visual-forward forward)
	(evil-god-toggle-stop-execute-in-god-state 'visual))
    (evil-god-toggle-stop-execute-in-god-state alternate_target)))

;;;###autoload
(defun evil-god-toggle-once ()
  "Enter God mode for exactly one command, then return to Evil.
If already in `god-once', signal a user-error."
  (interactive)
  (cond
   ((minibufferp)
    (user-error "Cannot enter god-once mode from minibuffer"))
   ((eq evil-state 'god-once)
    (user-error "Already in god-once state"))
   ((eq evil-state 'god)
    (user-error "Already in god state"))
   (t (evil-god-once-state))))

;;;###autoload
(defun evil-god-toggle-bail ()
  "Abort any God state immediately and return to Evil normal."
  (interactive)
  ;; exit God
  (evil-god-toggle--stop-hook-fun)
  (evil-god-toggle-stop-execute-in-god-state 'normal))

(evil-define-state god
  "God state." :tag " <G> "
  :message "-- GOD MODE --"
  :entry-hook  evil-god-toggle--start-hook-fun
  :exit-hook   evil-god-toggle--stop-hook-fun
  :input-method t :intercept-esc nil)

(evil-define-state god-once
  "God state (once)." :tag " <G (once)> "
  :message "-- GOD MODE (once) --"
  :entry-hook  evil-god-toggle--once-start-hook-fun
  :exit-hook   evil-god-toggle--once-stop-hook-fun
  :input-method t :intercept-esc nil)

(evil-define-state god-off
  "God state (off)." :tag " <G (off)> "
  :message "-- GOD MODE (OFF) --"
  :entry-hook  evil-god-toggle--off-start-hook-fun
  :exit-hook   evil-god-toggle--off-stop-hook-fun
  :input-method t :intercept-esc nil)


(defun evil-god-toggle--remove-transient-hooks ()
  "Remove God mode transient hooks from all buffers that added them."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when evil-god-toggle--has-fix-last-command-hook
        (remove-hook 'pre-command-hook #'evil-god-toggle--fix-last-command t)
        (setq-local evil-god-toggle--has-fix-last-command-hook nil))
      (when evil-god-toggle--has-exit-once-hook
        (remove-hook 'post-command-hook #'evil-god-toggle--exit-once t)
        (setq-local evil-god-toggle--has-exit-once-hook nil)))))

(defun evil-god-toggle--add-fix-last ()
  "Add `evil-god-toggle--fix-last-command` to `pre-command-hook`."
  (unless   (bound-and-true-p evil-god-toggle--has-fix-last-command-hook)
    (add-hook 'pre-command-hook #'evil-god-toggle--fix-last-command nil t)
    (setq-local evil-god-toggle--has-fix-last-command-hook t)))

(defun evil-god-toggle--add-exit-once ()
  "Add `evil-god-toggle--exit-once` to `post-command-hook`."
  (unless  (bound-and-true-p evil-god-toggle--has-exit-once-hook)
    (add-hook 'post-command-hook #'evil-god-toggle--exit-once nil t)
    (setq-local evil-god-toggle--has-exit-once-hook t)))


(defun evil-god-toggle--check-and-update-previous-state-visual ()
  "Set the previous Evil state to `normal' if it was `god' upon entering `visual'.
When Evil transitions into `visual' state from `god', it may
incorrectly keep `god' as the previous state.  This function checks
if `evil-previous-state' is `god', resets it to `normal', and updates
`evil-previous-state-alist' accordingly.  Put this on
`evil-visual-state-entry-hook' to ensure Evil retains the correct
previous state."
  (when (eq evil-previous-state 'god)
    (setq evil-previous-state 'normal)
    (setq evil-previous-state-alist
          (assq-delete-all 'god evil-previous-state-alist))
    (add-to-list 'evil-previous-state-alist (cons 'god 'normal))))


(defun evil-god-toggle--restore-visual-hooks ()
  "Re-add Evil visual activate/deactivate hooks where they were previously removed."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and
             (bound-and-true-p evil-local-mode)
             (bound-and-true-p evil-god-toggle--visual-hooks-removed))
        (evil-god-toggle--add-visual-hooks)
        (setq-local evil-god-toggle--visual-hooks-removed nil)))))

(defun evil-god-toggle--disable-god ()
  "Disable God mode, globally or buffer-locally per `evil-god-toggle-global'."
  (if evil-god-toggle-global
      (god-mode-all -1)
    (god-local-mode -1)))

(defun evil-god-toggle--enable-god ()
  "Enable God mode, either globally or buffer-locally per `evil-god-toggle-global'."
  (if evil-god-toggle-global
      (god-mode-all 1)
    (god-local-mode 1)))

(defun evil-god-toggle--start-hook-fun ()
  "Run before entering `evil-god-state'."
  (when (or (member #'evil-visual-activate-hook activate-mark-hook)
          (member #'evil-visual-deactivate-hook deactivate-mark-hook))
    (evil-god-toggle--remove-visual-hooks))
  ;; either global or local God
  (evil-god-toggle--enable-god))

(defun evil-god-toggle--stop-hook-fun ()
  "Run before exiting `evil-god-state'."
  ;; restore visual hooks, then turn off God
  (evil-god-toggle--remove-transient-hooks)
  (evil-god-toggle--restore-visual-hooks)
  (evil-god-toggle--disable-god))


(defun evil-god-toggle--fix-last-command ()
"Internal: Restore `last-command` captured before entering God state."
        (setq last-command evil-god-toggle--last-command)
        (remove-hook 'pre-command-hook #'evil-god-toggle--fix-last-command))

(defun evil-god-toggle--once-start-hook-fun ()
  "Run before entering `evil-god-once-state'."
 (setq evil-god-toggle--last-command last-command
       evil-god-toggle--once-buffer (current-buffer))
 (evil-god-toggle--remove-visual-hooks)
  ;; either global or local God
 (evil-god-toggle--add-fix-last)
 (evil-god-toggle--add-exit-once)
  (evil-god-toggle--enable-god))

(defun evil-god-toggle--once-stop-hook-fun ()
  "Run before exiting `evil-god-once-state'."
  ;; restore visual hooks, then turn off God
  (evil-god-toggle--remove-transient-hooks)
  (evil-god-toggle--restore-visual-hooks)
  (evil-god-toggle--disable-god))

(defun evil-god-toggle--off-start-hook-fun ()
  "Run before entering `evil-god-off-state'."
  (evil-god-toggle--remove-visual-hooks)
  ;; either global or local God
  (evil-god-toggle--disable-god))

(defun evil-god-toggle--off-stop-hook-fun ()
  "Run before exiting `evil-god-off-state'."
  ;; same cleanup for God-off exit
  (evil-god-toggle--restore-visual-hooks)
  (evil-god-toggle--disable-god))

(defun evil-god-toggle--maybe-restore-visual (next-state-fn)
  "Switch to NEXT-STATE-FN and optionally restore region if in visual mode."
  (if (and (evil-visual-state-p)
           (memq evil-god-toggle-persist-visual '(always to-god)))
      (let ((mrk (mark))
            (pnt (point)))

        (funcall next-state-fn)
        (set-mark mrk)
        (goto-char pnt))
    ( funcall next-state-fn)))

(defun evil-god-toggle--remove-visual-hooks ()
  "Remove Evil's visual activate/deactivate hooks in current buffer.
Prevents Evil's visual selection hooks from firing while in God mode.
`evil-god-toggle--visual-hooks-removed' set to non-nil so we know to restore."
  (when (or (member #'evil-visual-activate-hook activate-mark-hook)
          (member #'evil-visual-deactivate-hook deactivate-mark-hook))
  (remove-hook 'activate-mark-hook #'evil-visual-activate-hook t)
  (remove-hook 'deactivate-mark-hook #'evil-visual-deactivate-hook t)
  (setq-local evil-god-toggle--visual-hooks-removed t)))


(defun evil-god-toggle--add-visual-hooks ()
  "Re-add Evil's visual activate/deactivate hooks in the current buffer.
Restores visual selection behavior by adding `evil-visual-activate-hook' to
`activate-mark-hook', `evil-visual-deactivate-hook', to `deactivate-mark-hook'."
  (add-hook 'activate-mark-hook   #'evil-visual-activate-hook   nil t)
  (add-hook 'deactivate-mark-hook #'evil-visual-deactivate-hook nil t))


(defun evil-god-toggle--transition-to-normal ()
"Transition to evil normal state from god state."
  (when (region-active-p)
    (deactivate-mark))
(evil-normal-state))

(defun evil-god-toggle--transition-to-insert ()
"Transition to insert mode and ensure no region is highlighted."
  (when (region-active-p)
    (deactivate-mark))
  (evil-insert-state))

(defun evil-god-toggle--transition-to-visual ()
  "Enter Evil visual (charwise), restoring any stashed region & orientation."
  (when (region-active-p)
    (deactivate-mark))
  (if (and evil-god-toggle--visual-beg
           evil-god-toggle--visual-end)
      (progn
        (evil-visual-state)
        (evil-visual-select
         evil-god-toggle--visual-beg
         evil-god-toggle--visual-end
         'char)
        ;; swap point and mark if original was backward
        (unless evil-god-toggle--visual-forward
          (evil-exchange-point-and-mark))
        ;; clear the stash
        (setq evil-god-toggle--visual-beg nil
              evil-god-toggle--visual-end nil
              evil-god-toggle--visual-forward nil))
    ;; fallback: exactly like pressing `v` in normal
    (evil-visual-char)))


(defun evil-god-toggle--exit-once ()
  "Exit `god-once` state, return to previous Evil state, or `normal' if ambiguous."
  (let ((cmd this-command)
        (entry-cmd 'evil-god-once-state))
    (unless (memq cmd
                  ;; never exit on the toggle or the state-entry commands,
                  ;; nor on any of the built-in prefix arguments, or minibuffer
                  (list
                   'evil-god-toggle-once
                   entry-cmd
                   'universal-argument
                   'universal-argument-minus
                   'universal-argument-more
                   'universal-argument-other-key
                   'digit-argument
                   'negative-argument))
                   ;; also guard against being in the minibuffer:
                   ;; `minibufferp` is a predicate, not a command, so check it separately
      (unless (minibufferp)
        (evil-change-to-previous-state)))))


(provide 'evil-god-toggle)
;;; evil-god-toggle.el ends here
