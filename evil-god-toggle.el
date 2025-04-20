;;; evil-god-toggle.el --- Toggle between Evil and God Modes -*- lexical-binding: t -*-
;;
;; Copyright (C) 2023 Jordan Mandel
;; Author: Jordan Mandel <jordan.mandel@live.com>
;; Created: 2023 Summer
;; Version: 0.1
;; Package-Requires: ((emacs "24.1") (evil "1.0.8") (god-mode "2.12.0"))
;; Keywords: convenience, emulation, evil, god-mode
;; URL: https://github.com/jam1015/evil-god-toggle
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
;; This package toggles between Evil mode and God mode, enabling users to switch seamlessly.
;; Provided states:
;; - `god`: Full persistent God mode.
;; - `god-once`: Enter God mode for exactly one command, then automatically return to Evil.
;; - `god-off`: Explicitly turns God mode off (useful for temporary forced states).
;;
;; Ideal for users blending Emacs and Vim-style modal editing.
;;
;;; Code:






(defgroup evil-god-toggle nil
  "Customization group for evil-god-toggle."
  :group 'convenience
  :prefix "evil-god-toggle-")

(defcustom evil-god-toggle-persist-visual 'always
  "Control persistence of visual selection when toggling.
Possible values are:
- 'always: Persist visual selection both ways.
- 'to-god: Persist only when switching to God mode.
- 'to-evil: Persist only when switching to Evil mode.
- nil: Never persist."
  :type '(choice (const always) (const to-god) (const to-evil) (const nil))
  :group 'evil-god-toggle)



(defcustom evil-god-toggle-global nil
  "If non‑nil, use `god-mode-all` instead of `god-local-mode` (per‑buffer)."
  :type 'boolean
  :group 'evil-god-toggle)

(defvar evil-god-toggle--origin-buffer nil
  "Buffer where `evil-god-once-state` was entered, used for safe restoration.")
(defvar evil-god-toggle--visual-forward nil
  "Non-nil if the saved region was selected forward (mark ≤ point).")
(defvar-local evil-god-toggle--visual-beg nil
  "Buffer position of region start to restore when toggling back to Evil visual.")
(defvar-local evil-god-toggle--visual-end nil
  "Buffer position of region end to restore when toggling back to Evil visual.")

(require 'evil)
(require 'god-mode)

(evil-define-state god
  "God state."
  :tag " <G> "
  :message "-- GOD MODE --"
  :entry-hook (evil-god-toggle-start-hook-fun)
  :exit-hook (evil-god-toggle-stop-hook-fun)
  :input-method t
  :intercept-esc nil)


(evil-define-state god-once
  "God state (once)."
  :tag " <G (once)> "
  :message "-- GOD MODE (once) --"
  :entry-hook (evil-god-toggle-once-start-hook-fun)
  :exit-hook (evil-god-toggle-once-stop-hook-fun)
  :input-method t
  :intercept-esc nil)


(evil-define-state god-off
  "God state (off)."
  :tag " <G (off)> "
  :message "-- GOD MODE (OFF) --"
  :entry-hook (evil-god-off-toggle-start-hook-fun)
  :exit-hook (evil-god-off-toggle-stop-hook-fun)
  :input-method t
  :intercept-esc nil)


(defun evil-god-toggle-check-and-update-previous-state-visual ()
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

(add-hook 'evil-visual-state-entry-hook 'evil-god-toggle-check-and-update-previous-state-visual )



;;;;; helper functions ;;;;;;

(defvar-local evil-god-toggle--hooks-removed nil
  "Non-nil if evil-god-toggle removed visual mark hooks in this buffer.")


(defun evil-god-toggle--restore-visual-hooks ()
  "Re-add Evil visual activate/deactivate hooks where they were previously removed."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (bound-and-true-p evil-local-mode)
                 evil-god-toggle--hooks-removed)
        (add-hook 'activate-mark-hook   'evil-visual-activate-hook   nil t)
        (add-hook 'deactivate-mark-hook 'evil-visual-deactivate-hook nil t)
        (setq evil-god-toggle--hooks-removed nil)))))


(defun evil-god-toggle--disable-god ()
  "Disable God mode, either globally or buffer‑locally per `evil-god-toggle-global’."
  (if evil-god-toggle-global
      (god-mode-all -1)
    (god-local-mode -1)))


;;  god mode hooks ------------------------

(defun evil-god-toggle-start-hook-fun ()
  "Run before entering `evil-god-state'."
  (when (or (member #'evil-visual-activate-hook activate-mark-hook)
          (member #'evil-visual-deactivate-hook deactivate-mark-hook))
  (remove-hook 'activate-mark-hook 'evil-visual-activate-hook t)
  (remove-hook 'deactivate-mark-hook 'evil-visual-deactivate-hook t)
  (setq evil-god-toggle--hooks-removed t))
  ;; either global or local God
  (if evil-god-toggle-global
      (god-mode-all 1)   ; turn on God everywhere
    (god-local-mode 1)))

(defun evil-god-toggle-stop-hook-fun ()
  "Run before exiting `evil-god-state’."
  ;; restore visual hooks, then turn off God
  (evil-god-toggle--restore-visual-hooks )
  (evil-god-toggle--disable-god))





(defun evil-god-toggle-once-start-hook-fun ()
  "Run before entering `evil-god-once-state'."
    (setq evil-god-toggle--origin-buffer (current-buffer))
    (add-hook 'pre-command-hook #'evil-god-toggle-fix-last-command)
    (add-hook 'post-command-hook #'evil-god-toggle--exit-once)
  (when (or (member #'evil-visual-activate-hook activate-mark-hook)
          (member #'evil-visual-deactivate-hook deactivate-mark-hook))
  (remove-hook 'activate-mark-hook 'evil-visual-activate-hook t)
  (remove-hook 'deactivate-mark-hook 'evil-visual-deactivate-hook t)
  (setq evil-god-toggle--hooks-removed t))
  ;; either global or local God
  (if evil-god-toggle-global
      (god-mode-all 1)   ; turn on God everywhere
    (god-local-mode 1)))

(defun evil-god-toggle-once-stop-hook-fun ()
  "Run before exiting `evil-god-once-state’."
  ;; restore visual hooks, then turn off God
  (remove-hook 'pre-command-hook  #'evil-god-toggle-fix-last-command)
  (remove-hook 'post-command-hook #'evil-god-toggle--exit-once)
  (evil-god-toggle--restore-visual-hooks )
  (evil-god-toggle--disable-god))





;;  god mode (off) hooks ------------------------
(defun evil-god-off-toggle-start-hook-fun ()
  "Run before entering `evil-god-off-state'."
  (remove-hook 'activate-mark-hook 'evil-visual-activate-hook t)
  (remove-hook 'deactivate-mark-hook 'evil-visual-deactivate-hook t)
  ;; either global or local God
  (if evil-god-toggle-global
      (god-mode-all -1)   ; turn on God everywhere
    (god-local-mode -1)))


(defun evil-god-off-toggle-stop-hook-fun ()
  "Run before exiting `evil-god-off-state’."
  ;; same cleanup for God‑off exit
  (evil-god-toggle--restore-visual-hooks )
  (evil-god-toggle--disable-god))



(defvar evil-god-toggle-last-command nil
  "Command executed just before entering god state.")

(defvar evil-god-toggle-ran-first-evil-command nil
  "Non-nil if the first evil command ran after switching modes.")

;;------------------------------ the actual toggle function -----------------------------------

;;;###autoload
(defun evil-god-toggle ()
  "Toggle between God mode and Evil mode.
Handle visual selections and custom transitions."
  (interactive)
  (if (minibufferp)
      (user-error "Cannot enter God mode from minibuffer")
    (cond
     ;; Handle toggling from God mode to another Evil state
     ((eq evil-state 'god)
        (evil-god-toggle-execute-in-god-off-state))
     ;; Default case for any other states
     (t (evil-god-toggle-execute-in-god-state)))))



(defun evil-god-toggle--maybe-restore-visual (next-state-fn)
  "Switch to NEXT-STATE-FN and optionally restore region if in visual mode."
  (if (and (evil-visual-state-p)
           (memq evil-god-toggle-persist-visual '(always to-god)))
      (let ((mrk (mark))
            (pnt (point)))
        (funcall next-state-fn)
        (set-mark mrk)
        (goto-char pnt))
    (funcall next-state-fn)))



(defun evil-god-toggle-execute-in-god-off-state ()
  "Exit God mode, optionally restoring visual selection."
  (interactive)
  (add-hook 'pre-command-hook  #'evil-god-toggle-fix-last-command t)
  (setq evil-god-toggle-last-command last-command)
  (evil-god-toggle--maybe-restore-visual #'evil-god-off-state))


;; ------------------------ actually execute in god state --------------------------------
(defun evil-god-toggle-execute-in-god-state ()
  "Go into God state, preserving visual selection if configured."
  (interactive)
  (add-hook 'pre-command-hook  #'evil-god-toggle-fix-last-command t)
  (setq evil-god-toggle-last-command last-command)
  (evil-god-toggle--maybe-restore-visual #'evil-god-state))

(defun evil-god-toggle-fix-last-command ()
"Change `last-command' to be the command before `evil-god-toggle-execute-in-god-state'."
        (setq last-command evil-god-toggle-last-command)
        (remove-hook 'pre-command-hook 'evil-god-toggle-fix-last-command))


;; -------------------------------- stop god state and go to a different desired state -----------------

(defun evil-god-toggle-stop-execute-in-god-state (target)
  "Wrapper for leaving god state and switching to another Evil state based on TARGET.
TARGET should be a symbol: 'normal, 'insert, or 'visual."
  (interactive)
  (remove-hook 'pre-command-hook 'evil-god-toggle-fix-last-command)
  (pcase target
    ('normal (evil-god-toggle-transition-to-normal))
    ('insert (evil-god-toggle-transition-to-insert))
    ('visual (evil-god-toggle-transition-to-visual))
    (_       (evil-god-toggle-transition-to-normal))) ;; fallback
  (force-mode-line-update))


(defun evil-god-toggle-transition-to-normal ()
"Transition to evil normal state from god state."
  (when (region-active-p)
    (deactivate-mark))
(evil-normal-state))

(defun evil-god-toggle-transition-to-insert ()
"Transition to insert mode and ensure no region is highlighted."
  (when (region-active-p)
    (deactivate-mark))
  (evil-insert-state))

(defun evil-god-toggle-transition-to-visual ()
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


;;;###autoload
(defun evil-god-toggle-stop-choose-state (alternate_target)
  "From God, toggle back into Evil, restoring visual or going to normal.
If there’s an active region AND either persist‑visual flag is t,
stash its bounds **and** direction, then call visual; else normal."
  (interactive)
  (if (and (use-region-p)  (memq evil-god-toggle-persist-visual '(always to-evil))) ;; only if a region is selected
      (let* ((m       (mark))
             (p       (point))
             (beg     (min m p))
             ;; Emacs’s region-end is exclusive, so -1 to make it inclusive
             (end     (1- (max m p)))
             (forward (<= m p)))                ; mark before point?
        (setq evil-god-toggle--visual-beg     beg
              evil-god-toggle--visual-end     end
              evil-god-toggle--visual-forward forward)(evil-god-toggle-stop-execute-in-god-state 'visual))
    (evil-god-toggle-stop-execute-in-god-state alternate_target)))




(defun evil-god-toggle-once ()
  (interactive)
  (if (minibufferp)
      (user-error "Cannot enter God-once mode from minibuffer")
    (evil-god-once-state)))

(defun evil-god-toggle--exit-once ()
  "Exit `god-once` state and return to Evil's previous state, or `normal` if ambiguous."
  (let ((cmd this-command))
    (unless (or (eq cmd #'evil-god-toggle-once)
                (memq cmd '(universal-argument digit-argument))
                (minibufferp))
      ;; turn off God mode and restore previous state
      (let* ((raw-target (or (cdr (assq 'god-once evil-previous-state-alist))
                             evil-previous-state
                             'normal))
             (target (if (memq raw-target '(god god-once god-off nil))
                         'normal
                       raw-target)))
        (evil-change-state target)))))

;;;###autoload
(defun evil-god-toggle-bail ()
  "Abort any God state immediately and return to Evil normal."
  (interactive)
  ;; clean up any one‑shot hooks
  (remove-hook 'pre-command-hook  #'evil-god-toggle-fix-last-command t)
  (remove-hook 'post-command-hook #'evil-god-toggle--exit-once t)
  ;; exit God
  (evil-god-toggle-stop-hook-fun)
  (evil-normal-state)
  (message "-- Aborted God mode --"))

(provide 'evil-god-toggle)
;;; evil-god-toggle.el ends here
