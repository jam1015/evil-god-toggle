;;; evil-god-toggle.el --- Toggle between Evil and God modes easily

;; Copyright (C) 2023 Your Name
;; Author: Your Name <jordan.mandel@live.com>
;; URL: https://github.com/yourusername/evil-god-toggle
;; Inspired by: evil-god-state (https://github.com/gridaphobe/evil-god-state)
;; This is a heavily modified version of evil-god-state by gridaphobe.
;; License: GNU General Public License version 3, or (at your option) any later version

;;; Commentary:

;; The `evil-god-toggle` package allows seamless toggling between evil-mode and god-mode.
;; It is particularly useful for users who like both Vim and Emacs keybindings.
;;
;; Installation:
;; MELPA: M-x package-install RET evil-god-toggle
;; Manual: Clone this repo and add the following to your config:
;; (add-to-list 'load-path "/path/to/evil-god-toggle")
;; (require 'evil-god-toggle)
;;
;; Usage:
;; Toggle to god-mode: C-;
;; Toggle back to evil-mode: C-,
;;
;; Customization:
;; You can customize entry strategy, visual selection persistence, and state cursors.
;; Refer to README for more details.
;;
;; Best Practices:
;; - Use this package if you're well-acquainted with both Vim and Emacs keybindings.
;; - Customize the toggle keys to fit your workflow.

;;; Code:


(defgroup god-mode nil
  "Customization group for god-mode."
  :prefix "god-"
  :group 'convenience)

(defcustom god-entry-strategy "default"
  "Controls how the cursor moves when entering god-mode."
  :type '(choice 
                 (const :tag "Same as Vim" "default")
                 (const :tag "Stay in the same place" "same")
                 (const :tag "Toggle position" "toggle")
                 (const :tag "Reverse direction" "reverse")
                  )

  :group 'god-mode)


(defcustom persist_visual nil
  "Determines whether to persist the visual selection when switching modes.
When non-nil, the visual selection will persist."
  :type 'boolean
  :group 'god-mode)

(defcustom persist_visual_to_evil nil
  "Determines whether to persist the visual selection when switching to Evil mode.
When non-nil, the visual selection will persist."
  :type 'boolean
  :group 'god-mode)

(defcustom persist_visual_to_god nil
  "Determines whether to persist the visual selection when switching to God mode.
When non-nil, the visual selection will persist."
  :type 'boolean
  :group 'god-mode)

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
    (add-to-list 'evil-previous-state-alist (cons 'god 'normal))
    ;;(add-to-list 'evil-previous-state-alist (cons 'visual 'normal))
	)
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


  (unless (or 'persist_visual 'persist_visual_to_god ) (deactivate-mark))
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
   ((and (evil-visual-state-p) (or 'persist_visual 'persist_visual_to_god) )
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
     ((and mark-active (or 'persist_visual 'persist_visual_to_evil) ) 

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

(defun move_before_insert (append)
  "Enter god mode based on the value of `god_entry_strategy'.
If APPEND is non-nil, the behavior may be affected."
  (cond
   ((string-equal god_entry_strategy "same")
    (unless append
      (backward-char)))
   ((string-equal god_entry_strategy "toggle")
    (if append
	(backward-char)))
   ((string-equal god_entry_strategy "reverse")
    ;; TODO: Implement "reverse" strategy
    )
   (t
    (backward-char))))

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
	(move_before_insert append)	
	) ;; end unless
      (evil-execute-in-god-state )
      ) ;; end progn
    )
  )

(provide 'evil-god-toggle)
;;; evil-god-state.el ends here
