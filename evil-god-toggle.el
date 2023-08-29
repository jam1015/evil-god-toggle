;;; evil-god-state.el --- use god-mode keybindings in evil-mode

;; Copyright (C) 2023 by Jordan Mandel
;; Author: Jordan Mandel
;; URL: https://github.com/gridaphobe/evil-god-toggle
;; Filename: evil-god-toggle.el
;; Description: persistently toggle between god mode bindings and evil mode
;; Version: 0.1
;; Keywords: evil leader god-mode
;; Package-Requires: ((evil "1.0.8") (god-mode "2.12.0"))

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This is an evil-mode state for using god-mode.

;; It provides a command `evil-execute-in-god-state' that switches to
;; `god-local-mode' for the next command. I bind it to ","
;;
;;     (evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
;;
;; for an automatically-configured leader key.
;;
;; Since `evil-god-state' includes an indicator in the mode-line, you may want
;; to use `diminish' to keep your mode-line uncluttered, e.g.
;;
;;     (add-hook 'evil-god-state-entry-hook (lambda () (diminish 'god-local-mode)))
;;     (add-hook 'evil-god-state-exit-hook (lambda () (diminish-undo 'god-local-mode)))

;; It's handy to be able to abort a `evil-god-state' command.  The following
;; will make the <ESC> key unconditionally exit evil-god-state.
;;     (evil-define-key 'god global-map [escape] 'evil-god-state-bail)

;;; Code:
(require 'evil)
(require 'god-mode)

(evil-define-state god
  "God state."
  :tag " <G> "
  :message "-- GOD MODE --"
  :entry-hook (evil-god-start-hook)
  :exit-hook (evil-god-stop-hook)
  :input-method t
  :intercept-esc nil)

;; when entering visual state checks if previous state was god; if it was make the previous state be normal
(defun check-and-update-previous-state-visual ()
  (when (eq evil-previous-state 'god)
    (setq evil-previous-state 'normal)
    (setq evil-previous-state-alist
          (assq-delete-all 'god evil-previous-state-alist))
    (add-to-list 'evil-previous-state-alist (cons 'god 'normal)))
  )

(add-hook 'evil-visual-state-entry-hook 'check-and-update-previous-state-visual)

;; we're getting rid of these hooks because we don't want to activate visual mode when we highlight text in god mode
(defun evil-god-start-hook ()
  "Run before entering `evil-god-state'."
  (remove-hook 'activate-mark-hook 'evil-visual-activate-hook t) 
  (remove-hook 'deactivate-mark-hook 'evil-visual-deactivate-hook t)

  (god-local-mode 1))

;; putting the visual state hooks back and runing them if necessary
(defun evil-god-stop-hook ()
  "Run before exiting `evil-god-state'."

  ;; For evil-visual-deactivate-hook
  (unless (memq #'evil-visual-deactivate-hook (default-value 'deactivate-mark-hook))
	(add-hook 'deactivate-mark-hook #'evil-visual-deactivate-hook nil t))

  ;; For evil-visual-activate-hook
  (unless (memq #'evil-visual-activate-hook (default-value 'activate-mark-hook))
	(add-hook 'activate-mark-hook #'evil-visual-activate-hook nil t))


  (unless persist_visual (deactivate-mark))
  (god-local-mode -1)
  ) ; Restore the keymap

(defvar evil-god-last-command nil) ; command before entering evil-god-state
(defvar ran-first-evil-command nil)

( defun evil-god-fix-last-command ()
  "Change `last-command' to be the command before `evil-execute-in-god-state'."
(unless ran-first-evil-command
  (setq last-command evil-god-last-command)
  (setq ran-first-evil-command t)
  (remove-hook 'pre-command-hook 'evil-god-fix-last-command))
  )

;;;###autoload
(defun evil-execute-in-god-state ()
  "Go into god state, as if it is normal mode"
  (interactive)
  (setq ran-first-evil-command nil)
  (add-hook 'pre-command-hook  #'evil-god-fix-last-command t) ; (setq last-command evil-god-last-command))
  (setq evil-execute-in-god-state-buffer (current-buffer))
  (setq evil-god-last-command last-command)
  (cond
   ((and (evil-visual-state-p) persist_visual)
    ( let ((mrk (mark))
      (pnt (point)))
      (evil-god-state 1)
      (set-mark mrk)
      (goto-char pnt))
    )
   (t
    (evil-god-state 1)))
  )



;;;###autoload
(defun evil-stop-execute-in-god-state (to_insert)
  (interactive)
  "Switch back to previous evil state."
  (unless (or (eq this-command #'evil-execute-in-god-state)
			  (eq this-command #'universal-argument)
			  (eq this-command #'universal-argument-minus)
			  (eq this-command #'universal-argument-more)
			  (eq this-command #'universal-argument-other-key)
			  (eq this-command #'digit-argument)
			  (eq this-command #'negative-argument)
			  (minibufferp))
	(remove-hook 'pre-command-hook 'evil-god-fix-last-command)
	(cond 
	  (to_insert (evil-insert-state 1))
		  ((and mark-active persist_visual) 

											 (unless (memq #'evil-visual-deactivate-hook (default-value 'deactivate-mark-hook))
											   (add-hook 'deactivate-mark-hook #'evil-visual-deactivate-hook nil t))

											 ;; For evil-visual-activate-hook
											 (unless (memq #'evil-visual-activate-hook (default-value 'activate-mark-hook))
											   (add-hook 'activate-mark-hook #'evil-visual-activate-hook nil t))

											 (force-mode-line-update)
											 (evil-visual-state)
											 (force-mode-line-update)
											 )
		  (t 
			(message "going to normal")
			( evil-normal-state )
		   )


		  )

	
	)

  )


;;;###autoload
(defun god-toggle (append)
  (interactive)
  ;; in god local mode; switch to evil-insert and move cursor if append is true
  (if god-local-mode 
	(progn
	  (evil-echo "Switching out of evil-god-mode")
	  (evil-stop-execute-in-god-state t) ; going into insert mode; mapping escape to go to god mode
	  (
	   if append (forward-char)); like pressing 'a' in normal mode in vim, otherwise it's live you've pressed 'i'
	  )
	;; not in god-local-mode,  move the cursor based on user preference and then go into god-state
	(progn 
	  (evil-echo "Switching into evil-god-mode")
	  (unless (eq evil-state 'normal); don't move the cursor if we're in normal mode; entering god mode is like going into a different normal mode
		(cond
		  (
		   (string-equal god_entry_strategy "same") ;append will move the cursor forward when moving into evilmode
		   (unless append
			 (backward-char)
			 )
		   )
		  (
		   (string-equal god_entry_strategy "toggle") ; append undoes the effect of going into insert mode
		   (if append
			 (backward-char)
			 )
		   )
		  (
		   (string-equal god_entry_strategy "reverse") ; opposite of vim behavior
		   ;; doing nothing does the opposite of what vim normally does 
		   )

		  (t ;; default behafior for entering god mode; vim default behavior
			(backward-char)
			) ;; end cond
		  )
		) ;; end unless
	  (evil-execute-in-god-state )
	  ) ;; end progn
	)
 )

(provide 'evil-god-state)
;;; evil-god-state.el ends here
