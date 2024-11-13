;;; evil-god-toggle.el --- Toggle between Evil and God Modes -*- lexical-binding: t -*-
;; Copyright (C) 2023 Jordan Mandel
;; Author: Jordan Mandel <jordan.mandel@live.com>
;; Created: 2023 Summer
;; Version: 0.1
;; Keywords: evil god-mode
;; URL: https://github.com/jam1015/evil-god-toggle
;; Package-Requires: ((evil "1.0.8") (god-mode "2.12.0"))

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The `evil-god-toggle` package allows seamless toggling between evil-mode and god-mode.
;; It is particularly useful for users who like both Vim and Emacs keybindings.

;;; Code:

(defgroup evil-god-toggle nil
  "Customization group for evil-god-toggle."
  :group 'convenience
  :prefix "evil-god-toggle-")

(defcustom evil-god-toggle-persist-visual nil
           "If non-nil, retains the visual selection when toggling between God and Evil modes.
           This option automatically enables both `evil-god-toggle-persist-visual-to-evil' and 
           `evil-god-toggle-persist-visual-to-god', preserving the selection regardless of mode transition."
           :type 'boolean
           :group 'evil-god-toggle)

(defcustom evil-god-toggle-persist-visual-to-god nil
           "If non-nil, retains the visual selection when switching from Evil to God mode as an active region.
           Enabling `evil-god-toggle-persist-visual' will also enable this setting."
           :type 'boolean
           :group 'evil-god-toggle)


(defcustom evil-god-toggle-persist-visual-to-evil nil
               "If non-nil, retains the active region when switching from God to Evil mode, as a visual selection.
               Enabling `evil-god-toggle-persist-visual' will also enable this setting."
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

;; when entering visual state checks if previous state was god; if it was make the previous state be normal
;; this makes `escape' behave as expected
(defun check-and-update-previous-state-visual ()
  (when (eq evil-previous-state 'god)
    (setq evil-previous-state 'normal)
    (setq evil-previous-state-alist
          (assq-delete-all 'god evil-previous-state-alist))
    (add-to-list 'evil-previous-state-alist (cons 'god 'normal))))

(add-hook 'evil-visual-state-entry-hook 'check-and-update-previous-state-visual)

;; hook for starting god mode
(defun evil-god-start-hook-fun ()
  "Run before entering `evil-god-state'."

  ;; this function removes hooks because we don't want to activate visual mode when we highlight text in god mode
  (remove-hook 'activate-mark-hook 'evil-visual-activate-hook t) 
  (remove-hook 'deactivate-mark-hook 'evil-visual-deactivate-hook t)

  (god-local-mode 1)
  )

;; putting the visual state hooks back and runing them if necessary
(defun evil-god-stop-hook-fun ()
  "Run before exiting `evil-god-state'."

  ;; For evil-visual-activate-hook
  (unless (memq #'evil-visual-activate-hook (buffer-local-value 'activate-mark-hook (current-buffer)))
    (add-hook 'activate-mark-hook #'evil-visual-activate-hook nil t))

  ;; For evil-visual-deactivate-hook
  (unless (memq #'evil-visual-deactivate-hook (buffer-local-value 'deactivate-mark-hook (current-buffer)))
    (add-hook 'deactivate-mark-hook #'evil-visual-deactivate-hook nil t))

  (god-local-mode -1)
  ) ; Restore the keymap

(defvar evil-god-last-command nil) ; command before entering evil-god-state
(defvar ran-first-evil-command nil)

(defun god-toggle ()
  "Toggle between God mode and Evil mode, handling visual selections and custom transitions."
  (message "god-toggle called with evil-state: %s" evil-state)
  (cond
   ;; Handle toggling from God mode to another Evil state
   ((eq evil-state 'god)
  (message "in god mode conditinal")
    (cond
     ;; Handle visual selection when toggling from God to Evil mode
     ((and mark-active (or evil-god-toggle-persist-visual-to-evil evil-god-toggle-persist-visual))
  (progn
    (message "Rectangle mark mode not active: transitioning to visual state")
    (evil-stop-execute-in-god-state "visual")))
     ;; Default case to transition into insert mode
     (t (evil-stop-execute-in-god-state "insert"))))
 
   ;; Handle toggling from Normal or Insert state to God mode
   ((eq evil-state 'normal) (evil-execute-in-god-state))
   ((eq evil-state 'insert) (evil-execute-in-god-state))

   ;; Handle toggling from Visual, Visual Line, or Visual Block to God mode
   ((or (eq evil-state 'visual)  (eq evil-state 'visual-line))
    (if (or evil-god-toggle-persist-visual-to-god evil-god-toggle-persist-visual)
        (progn
          (evil-execute-in-god-state))
      (evil-execute-in-god-state)))

   ;; Default case for any other states
   (t (evil-execute-in-god-state))))

;;;###autoload
(defun evil-execute-in-god-state ()
  "Go into god state, as if it is normal mode"
  (interactive)
  (add-hook 'pre-command-hook  #'evil-god-fix-last-command t) ; (setq last-command evil-god-last-command))
  (setq evil-god-last-command last-command)
  (cond
    ((and (evil-visual-state-p) evil-god-toggle-persist-visual  )
     ( let ((mrk (mark))
            (pnt (point)))
           (evil-god-state )
           (set-mark mrk)
           (goto-char pnt))
     )
    (t
      (evil-god-state )))
  )

;;;###autoload
( defun evil-god-fix-last-command ()
        "Change `last-command' to be the command before `evil-execute-in-god-state'."
        (setq last-command evil-god-last-command)
        (remove-hook 'pre-command-hook 'evil-god-fix-last-command)
        )


(defun evil-stop-execute-in-god-state (target)
  "Stop God state and transition to the given Evil state, handling visual selections."
  (message "target state: %s" target)
  (remove-hook 'pre-command-hook 'evil-god-fix-last-command)
  (cond
   ((string= target "normal") (transition-to-normal))
   ((string= target "insert") (transition-to-insert))
   ((string= target "visual") (transition-to-visual))
   (t (transition-to-normal)))
  (force-mode-line-update))

(defun transition-to-normal ()
  (evil-normal-state)
  (when (use-region-p)
    (deactivate-mark)))

(defun transition-to-insert ()
  "Transition to insert mode and ensure no region is highlighted."
  (when (use-region-p)
    (deactivate-mark))
  (evil-insert-state))


(defun transition-to-visual ()
  "Transition to visual state, covering the active region if it exists.
If there is no active region, enter visual mode at the mark. Handles zero-length regions correctly."
  (if (region-active-p)
      ;; Active region exists
      (let* ((start (region-beginning))
             (end (region-end))
             (is-forward (<= start end)))
        ;; Adjust the endpoints for Evil's inclusive selection
        (if is-forward
            ;; Forward region: decrement end by 1
            (setq end (max (1- end) start))
          ;; Backward region: increment start by 1
          (setq start (min (1+ start) end)))
        ;; Exit God mode if necessary
        (when (bound-and-true-p god-local-mode)
          (evil-normal-state))
        ;; Use evil-visual-select to cover the adjusted region
        (evil-visual-select start end))
    ;; No active region or zero-length region
    (if (mark t)
        (progn
          ;; Exit God mode if necessary
          (when (bound-and-true-p god-local-mode)
            (evil-normal-state))
          ;; Enter visual mode at the mark position
          (evil-visual-state)
          (goto-char (mark t)))
      ;; If no mark is set, enter visual mode at point
      (when (bound-and-true-p god-local-mode)
        (evil-normal-state))
      (evil-visual-state)))
  ;; Update mode line
  (force-mode-line-update))


;; Assuming `evil-god-state-map` is the keymap for your custom God state,
;; you can bind C-z to switch to Evil Emacs state like this:
;; (define-key evil-god-state-map (kbd "C-z") 'switch-to-evil-emacs-state)

;; If the above doesn't work because `evil-god-state-map` isn't defined or
;; you want to ensure it works directly within God mode regardless of Evil's state,
;; you might want to add the binding directly to `god-local-mode-map` like this:
;;(eval-after-load 'god-mode '(define-key god-local-mode-map (kbd "C-z") 'switch-to-evil-emacs-state))

(provide 'evil-god-toggle)
;;; evil-god-toggle.el ends here
