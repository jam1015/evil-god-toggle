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
;; The `evil-god-toggle` package toggles between evil-mode and god-mode.
;; It is useful for users who like both Vim and Emacs keybindings.
;;
;;; Code:






(defgroup evil-god-toggle nil
  "Customization group for evil-god-toggle."
  :group 'convenience
  :prefix "god-")

(defcustom evil-god-toggle-persist-visual nil
  "Determines whether to persist the visual selection when switching modes.
When non-nil, the visual selection will persist. If non-nil, this implies
both `evil-god-toggle-persist-visual-to-evil' and
`evil-god-toggle-persist-visual-to-god'."
  :type 'boolean
  :group 'evil-god-toggle)

(defcustom evil-god-toggle-persist-visual-to-god nil
  "Determines whether to persist the visual selection when switching to God mode.
When non-nil, the visual selection will persist."
  :type 'boolean
  :group 'evil-god-toggle)


(defcustom evil-god-toggle-global nil
  "If non‑nil, use `god-mode-all` (global God) instead of `god-local-mode` (per‑buffer)."
  :type 'boolean
  :group 'evil-god-toggle)


(defcustom evil-god-toggle-persist-visual-to-evil nil
  "Determines whether to persist the visual selection when switching to Evil mode.
When non-nil, the visual selection will persist."
  :type 'boolean
  :group 'evil-god-toggle)

(defvar evil-god-toggle--visual-forward nil
  "Non-nil if the saved region was selected forward (mark ≤ point).")
(defvar evil-god-toggle--visual-beg nil
  "Buffer position of region start to restore when toggling back to Evil visual.")
(defvar evil-god-toggle--visual-end nil
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


(defun evil-god-toggle-start-hook-fun ()
  "Run before entering `evil-god-state'."
  (remove-hook 'activate-mark-hook 'evil-visual-activate-hook t)
  (remove-hook 'deactivate-mark-hook 'evil-visual-deactivate-hook t)
  ;; either global or local God
  (if evil-god-toggle-global
      (god-mode-all 1)   ; turn on God everywhere
    (god-local-mode 1)))

(defun evil-god-toggle-stop-hook-fun ()
  "Run before exiting `evil-god-state'."
  (unless (memq #'evil-visual-activate-hook
                (buffer-local-value 'activate-mark-hook (current-buffer)))
    (add-hook 'activate-mark-hook #'evil-visual-activate-hook nil t))
  (unless (memq #'evil-visual-deactivate-hook
                (buffer-local-value 'deactivate-mark-hook (current-buffer)))
    (add-hook 'deactivate-mark-hook #'evil-visual-deactivate-hook nil t))
  ;; either global or local God off
  (if evil-god-toggle-global
      (god-mode-all -1) ; turn off God everywhere
    (god-local-mode -1)))

(defvar evil-god-toggle-last-command nil
  "Command executed just before entering god state.")

(defvar evil-god-toggle-ran-first-evil-command nil
  "Non-nil if the first evil command ran after switching modes.")

;;------------------------------ the actual toggle function -----------------------------------
(defun evil-god-toggle ()
  "Toggle between God mode and Evil mode.
Handle visual selections and custom transitions."
  (cond
   ;; Handle toggling from God mode to another Evil state
   ((eq evil-state 'god)
      (evil-god-toggle-stop-execute-in-god-state "insert"))
   ;; Default case for any other states
   (t (evil-god-toggle-execute-in-god-state))))










;; ------------------------ actually execute in god state --------------------------------
(defun evil-god-toggle-execute-in-god-state ()
"Go into god state, as if it is normal mode."
  (interactive)
  (add-hook 'pre-command-hook  #'evil-god-toggle-fix-last-command t)
  (setq evil-god-toggle-last-command last-command)
  (cond
    ((and (evil-visual-state-p) (or evil-god-toggle-persist-visual-to-god evil-god-toggle-persist-visual )  )
     ( let ((mrk (mark))
            (pnt (point)))
           (evil-god-state )
           (set-mark mrk)
           (goto-char pnt)))
    (t
      (evil-god-state ))))

;;;###autoload
(defun evil-god-toggle-fix-last-command ()
"Change `last-command' to be the command before `evil-god-toggle-execute-in-god-state'."
        (setq last-command evil-god-toggle-last-command)
        (remove-hook 'pre-command-hook 'evil-god-toggle-fix-last-command))


;; -------------------------------- stop god state and go to a different desired state -----------------

(defun evil-god-toggle-stop-execute-in-god-state (target)
"Wrapper for leaving god state and switching to another Evil state based on TARGET."
  (remove-hook 'pre-command-hook 'evil-god-toggle-fix-last-command)
  (cond
   ((equal target "normal")
    (evil-god-toggle-transition-to-normal))
   ((equal target "insert")
    (evil-god-toggle-transition-to-insert))
   ((equal target "visual")
    (evil-god-toggle-transition-to-visual))
   (t
    (evil-god-toggle-transition-to-normal)))
  (force-mode-line-update))


(defun evil-god-toggle-transition-to-normal ()
"Transition to evil normal state from god state."
  (when (use-region-p)
    (deactivate-mark))
(evil-normal-state))

(defun evil-god-toggle-transition-to-insert ()
"Transition to insert mode and ensure no region is highlighted."
  (when (use-region-p)
    (deactivate-mark))
  (evil-insert-state))

(defun evil-god-toggle-transition-to-visual ()
  "Enter Evil visual (charwise), restoring any stashed region & orientation."
  (when (use-region-p)
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
        (setq evil-god-toggle--visual-beg
              evil-god-toggle--visual-end
              evil-god-toggle--visual-forward
              nil))
    ;; fallback: exactly like pressing `v` in normal
    (evil-visual-char)))


(defun evil-god-toggle-stop-choose-state ()
  "From God, toggle back into Evil, restoring visual or going to normal.
If there’s an active region AND either persist‑visual flag is t,
stash its bounds **and** direction, then call visual; else normal."
  (interactive)
  (if (and (use-region-p)
           (or evil-god-toggle-persist-visual
               evil-god-toggle-persist-visual-to-evil))
      (let* ((m       (mark))
             (p       (point))
             (beg     (min m p))
             ;; Emacs’s region-end is exclusive, so -1 to make it inclusive
             (end     (1- (max m p)))
             (forward (<= m p)))                ; mark before point?
        (setq evil-god-toggle--visual-beg     beg
              evil-god-toggle--visual-end     end
              evil-god-toggle--visual-forward forward)
        (evil-god-toggle-stop-execute-in-god-state "visual"))
    (evil-god-toggle-stop-execute-in-god-state "normal")))









;;;###autoload
(defun evil-god-toggle-once ()
  "Enter God state for exactly the next command, then return to Evil."
  (interactive)
  ;; preserve last-command so the next command still behaves like Evil
  (add-hook 'pre-command-hook #'evil-god-toggle-fix-last-command nil t)
  ;; after that one real command, clean up and switch back
  (add-hook 'post-command-hook #'evil-god-toggle--exit-once nil t)
  ;; enter God
  (if evil-god-toggle-global
      (god-mode-all 1)
    (god-local-mode 1))
  (message "-- GOD MODE (next cmd only) --"))

(defun evil-god-toggle--exit-once ()
  "Internal: exit God state once a non‑prefix command has run."
  (let ((cmd this-command))
    (unless (or
             ;; skip our own entry command
             (eq cmd #'evil-god-toggle-once)
             ;; skip any original `evil-execute-in-god-state`
             (eq cmd #'evil-god-toggle-execute-in-god-state)
             ;; skip prefix args
             (memq cmd '(universal-argument
                         universal-argument-minus
                         universal-argument-more
                         universal-argument-other-key
                         digit-argument
                         negative-argument))
             ;; skip in the minibuffer
             (minibufferp))
      ;; tear down hooks
      (remove-hook 'pre-command-hook  #'evil-god-toggle-fix-last-command t)
      (remove-hook 'post-command-hook #'evil-god-toggle--exit-once t)
      ;; run your normal exit hook and go to normal state
      (evil-god-toggle-stop-hook-fun)
      (evil-normal-state))))

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
