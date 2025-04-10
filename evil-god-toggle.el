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

(defcustom evil-god-toggle-persist-visual-to-evil nil
  "Determines whether to persist the visual selection when switching to Evil mode.
When non-nil, the visual selection will persist."
  :type 'boolean
  :group 'evil-god-toggle)

(require 'evil)
(require 'god-mode)

(evil-define-state god
  "God state."
  :tag " <G> "
  :message "-- GOD MODE --"
  :entry-hook (evil-god-start-hook-fun)
  :exit-hook (evil-god-stop-hook-fun)
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
(add-hook 'evil-visual-state-entry-hook 'check-and-update-previous-state-visual )

;; hook for starting god mode
(defun evil-god-toggle-start-hook-fun ()
  "Run before entering `evil-god-state'.
This function removes hooks because we don't
want to activate visual mode when
we highlight text in god mode"
  (remove-hook 'activate-mark-hook 'evil-visual-activate-hook t)
  (remove-hook 'deactivate-mark-hook 'evil-visual-deactivate-hook t)
  (god-local-mode 1))

;; putting the visual state hooks back and runing them if necessary
(defun evil-god-toggle-stop-hook-fun ()
  "Run before exiting evil-god-state.
Putting visual state hooks back,
they were removed when god state started"
  ;; For evil-visual-activate-hook
  (unless (memq #'evil-visual-activate-hook (buffer-local-value 'activate-mark-hook (current-buffer)))
    (add-hook 'activate-mark-hook #'evil-visual-activate-hook nil t))

  ;; For evil-visual-deactivate-hook
  (unless (memq #'evil-visual-deactivate-hook (buffer-local-value 'deactivate-mark-hook (current-buffer)))
    (add-hook 'deactivate-mark-hook #'evil-visual-deactivate-hook nil t))

  (god-local-mode -1)) ; Restore the keymap

(defvar evil-god-toggle-last-command nil
  "Command executed just before entering god state.")

(defvar evil-god-toggle-ran-first-evil-command nil
  "Non-nil if the first evil command ran after switching modes.")

(defun evil-god-toggle ()
  "Toggle between God mode and Evil mode.
Handle visual selections and custom transitions."
  (message "god-toggle called with evil-state: %s" evil-state)
  (cond
   ;; Handle toggling from God mode to another Evil state
   ((eq evil-state 'god)
      (evil-god-toggle-stop-execute-in-god-state "insert"))
   ;; Default case for any other states
   (t (evil-god-toggle-execute-in-god-state))))

;;  (cond ((eq evil-state 'god)(cond
;;                               ((and mark-active  (or evil-god-toggle-persist-visual-to-evil evil-god-toggle-persist-visual)) (  evil-god-toggle-stop-execute-in-god-state "visual" )
;;                                ;;(guarded-backward-char)
;;                                )
;;                               ;; forward char because there is an of-by-one difference between how emacs and evil deal with the selection
;;                               (t                                          (evil-god-toggle-stop-execute-in-god-state "insert") )
;;                               ))
;;        ((eq evil-state 'normal) (evil-god-toggle-execute-in-god-state))
;;        ((eq evil-state 'insert)(evil-god-toggle-execute-in-god-state))
;;        ((eq evil-state 'visual)(evil-god-toggle-execute-in-god-state))
;;        (t (evil-god-toggle-execute-in-god-state)))


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

;;(defun evil-god-toggle-transient-god-state ()
;;"Go into god state for one command."
;;  (interactive)
;;  (setq evil-god-toggle-execute-in-god-state-buffer (current-buffer))
;;  (add-hook 'post-command-hook #'t)
;;  (evil-god-toggle-execute-in-god-state))
;;
;;
;;(defun stop-transient-god-state ()
;;  (unless (or (eq this-command #'evil-execute-in-god-state)
;;              (eq this-command #'universal-argument)
;;              (eq this-command #'universal-argument-minus)
;;              (eq this-command #'universal-argument-more)
;;              (eq this-command #'universal-argument-other-key)
;;              (eq this-command #'digit-argument)
;;              (eq this-command #'negative-argument)
;;              (minibufferp))
;;    (remove-hook 'pre-command-hook 'evil-god-fix-last-command)
;;    (remove-hook 'post-command-hook 'evil-stop-execute-in-god-state)
;;    (when (buffer-live-p evil-execute-in-god-state-buffer)
;;      (with-current-buffer evil-execute-in-god-state-buffer
;;                           (if (and (eq evil-previous-state 'visual)
;;                                    (not (use-region-p)))
;;                             (progn
;;                               (evil-change-to-previous-state)
;;                               (evil-exit-visual-state))
;;                             (evil-change-to-previous-state))))
;;    (setq evil-execute-in-god-state-buffer nil))
;;  )
;;
;;;###autoload
(defun evil-god-toggle-fix-last-command ()
        "Change `last-command' to be the command before `evil-execute-in-god-state'."
        (setq last-command evil-god-toggle-last-command)
        (remove-hook 'pre-command-hook 'evil-god-fix-last-command))


(defun evil-god-toggle-stop-execute-in-god-state (target)
"Wrapper function for leaving god state to go to other evil states.
That state is determined by TARGET."
  (remove-hook 'pre-command-hook 'evil-god-fix-last-command)
  (cond
    ((string= target "normal")(evil-god-toggle-transition-to-normal))
    ((string= target "insert")(evil-god-toggle-transition-to-insert))
    ((string= target "visual")(evil-god-toggle-transition-to-visual))
    (t (evil-god-toggle-transition-to-normal)))
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
  "Transition to insert mode and ensure no region is highlighted."
  (when (use-region-p)
    (deactivate-mark))
  (evil-visual-state))

(provide 'evil-god-toggle)
;;; evil-god-toggle.el ends here
