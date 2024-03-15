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
					"Customization group for god-mode."
					:prefix "god-"
					:group 'convenience)

(defcustom insert-to-god-cursor-strategy "default"
					 "Controls how the cursor moves when entering god-mode."
					 :type '(choice 
										(const :tag "Same as Vim" "default")
										(const :tag "Stay in the same place" "same")
										(const :tag "Toggle position" "toggle")
										(const :tag "Reverse direction" "reverse")
										)
					 :group 'evil-god-toggle)

(defcustom persist_visual nil
					 "Determines whether to persist the visual selection when switching modes.
					 When non-nil, the visual selection will persist. If non-nil it implies both
					 persist_visual_to_evil and persist_visual_to_god.  These parameters are
					 logically related to each other by 'or'"
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
;; this makes `escape` behave as expected
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

	;;(unless  persist_visual  (deactivate-mark))
	(god-local-mode -1)
	) ; Restore the keymap

(defvar evil-god-last-command nil) ; command before entering evil-god-state
(defvar ran-first-evil-command nil)

(defun god-toggle (append)

	(cond ((eq evil-state 'god)(cond
															 ((and mark-active  persist_visual) (  evil-stop-execute-in-god-state "visual" )(guarded-backward-char))
															 (t                                          (evil-stop-execute-in-god-state "insert")    (when append (guarded-forward-char))    )
															 ))
				((eq evil-state 'normal) (evil-execute-in-god-state))
				((eq evil-state 'insert)(cursor_toggle_motion_insert_to_god append)(evil-execute-in-god-state))
				((eq evil-state 'visual)(evil-execute-in-god-state))
				(t (evil-execute-in-god-state))
				)

	)

;;;###autoload
(defun evil-execute-in-god-state ()
	"Go into god state, as if it is normal mode"
	(interactive)
	(add-hook 'pre-command-hook  #'evil-god-fix-last-command t) ; (setq last-command evil-god-last-command))
	(setq evil-god-last-command last-command)
	(cond
		((and (evil-visual-state-p) persist_visual  )
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

	(remove-hook 'pre-command-hook 'evil-god-fix-last-command)
	(cond
		((string= target "normal")(transition-to-normal))
		((string= target "insert")(transition-to-insert))
		((string= target "visual")(transition-to-visual))
		(t (transition-to-normal))
		)
	(force-mode-line-update)
	)

( defun transition-to-normal()
				( evil-normal-state )
				(when (use-region-p)
					(deactivate-mark))
				)

(defun transition-to-insert()
	(evil-insert-state)
	)
(defun transition-to-visual()
	(force-mode-line-update)
	(evil-visual-state)
	(force-mode-line-update)
	)


(defun guarded-forward-char ()
	"Move forward a character only if not at the end of a line."
	(when (< (point) (line-end-position))
		(forward-char)))

;;;###autoload
(defun guarded-backward-char ()
	"Move backward a character only if not at the beginning of a line."
	(when (> (point) (line-beginning-position))
		(backward-char)))

;;;###autoload
(defun cursor_toggle_motion_insert_to_god (append)
	"Enter insert mode based on the value of `insert-to-god-cursor-strategy'."
	;; Ensure insert-to-god-cursor-strategy is bound; set to nil if not.
	(unless (boundp 'insert-to-god-cursor-strategy)
		(setq insert-to-god-cursor-strategy nil))


	(cond
		;; Check if the strategy is explicitly set to "same"
		((string-equal insert-to-god-cursor-strategy "same")
		 (unless append
			 (guarded-backward-char)))

		;; Check if the strategy is set to "toggle"
		((string-equal insert-to-god-cursor-strategy "toggle")
		 (if append
			 (guarded-backward-char)))

		;; Placeholder for "reverse" strategy; define behavior if needed
		((string-equal insert-to-god-cursor-strategy "reverse")
		 ;; left blank intentionally
		 )

		;; Default case, including when insert-to-god-cursor-strategy is nil
		((or (null insert-to-god-cursor-strategy) t) (guarded-backward-char))
		)

	)



(defun switch-to-evil-emacs-state ()
	"Switch from God mode to Evil Emacs state."
	(interactive)
	(evil-emacs-state 1))

;; Assuming `evil-god-state-map` is the keymap for your custom God state,
;; you can bind C-z to switch to Evil Emacs state like this:
;; (define-key evil-god-state-map (kbd "C-z") 'switch-to-evil-emacs-state)

;; If the above doesn't work because `evil-god-state-map` isn't defined or
;; you want to ensure it works directly within God mode regardless of Evil's state,
;; you might want to add the binding directly to `god-local-mode-map` like this:
;;(eval-after-load 'god-mode '(define-key god-local-mode-map (kbd "C-z") 'switch-to-evil-emacs-state))


(provide 'evil-god-toggle)
;;; evil-god-toggle.el ends here
