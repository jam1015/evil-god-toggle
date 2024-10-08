# evil-god-toggle

A package to seamlessly toggle between
[evil-mode](https://github.com/emacs-evil/evil) and
[god-mode](https://github.com/chrisdone/god-mode).

Inspired by and a heavily modified version of
[evil-god-state](https://github.com/gridaphobe/evil-god-state)

## Background

Evil Mode provides very accurate Vim emulation in Emacs.

God mode modifies Emacs so that it operates as if the control key is being held down at all times, with further input strategies to emulate holding down other modifier keys. In my interpretation this is analogous to normal mode in Vim, where all non-insertion capabilities of the editor are similarly available without holding down modifier keys. Under this interpretation, Emacs Fundamental mode is like Insert Mode in Vim.

I wanted a good way to switch between Evil and God Mode. Evil-God-State does this, making God Mode a mode contained withing Evil Mode (the strategy used by `evil-god-state`, but switches back to Evil mode after one command.  This package makes the change to God-mode persistent, and provides functions to toggle between Evil Mode and God Mode.


***TAKE NOTE*** Through the rest of this README God Mode will refer to the sub mode of Evil Mode where God Mode is enabled.  It is a mode in Evil just like Normal Mode or Insert Mode.

## Installation

Use [straignt.el](https://github.com/radian-software/straight.el) to install from GitHub, or clone this repository and manually add it to your load-path. 

- **TODO**: Get this package on MELPA after community feedback.

## Usage

The two functions that the user interacts with are `god-toggle` and `evil-stop-execute-in-god-state`.

### `god-toggle`

Toggles between God Mode and Evil's insert mode, and from Normal Mode into God Mode.

It takes the argument: `append`.  The purpose of this argument is to modify what the cursor does when switching from Evil Mode to insert mode and vice-versa, in a way analogout to the differencet between entering insert mode by pressing `a` or `i` in Vim.

- If Emacs is in Normal mode and `god-toggle` is called, Emacs will enter God Mode and the cursor will remain in the same place. `append` had no effect.

- If Emacs is in God Mode of and this function is called, Emacs will enter Evil's insert mode. Further, if the argument `append` is non-nil the cursor will move one space forward as if `a` was pressed to enter insert Normal mode in Vim. 

- If Emacs is in Evil's insert mode and this function is called, Emacs will go into God Mode.  Further, the motion of the cursor is determined by the global variable `insert-to-god-cursor-strategy`

    - `default` (which is in fact the default) (or any value different from the next three options): Same cursor motion as switching from Insert mode to Normal mode in Vim. The cursor is moved one space back. Repeatedly calling `god-toggle` with `append` equal to `nil` will move the cursor backwards each cycle (the same as repeatedly preassing `i` and then `<esc>` over and over in Vim..  Repeatedly calling `god toggle` with `append` non-nil will move the cursor forwards and backwards in the same two positions, which is the same behavior as pressing `a` and `<esc>` over and over in Vim.


     - `same`: If `append` is `nil` then the cursor remains in the same place. If `append` is non-`nil` then the cursor moves back one space. This means that repeatedly calling the 

### `evil-stop-execute-in-god-state` (which is also called by `god-toggle`).

## Customization

### Keybindings

Customize the keybindings to fit your specific needs. For instance:

    (global-set-key (kbd "C-;") (lambda () (interactive) (god-toggle t)))

### Entry Strategy


You can customize the cursor behavior when entering god-mode using
`god_entry_strategy`. Choices include \"same\", \"toggle\", and
\"reverse\".


### Visual Selection


The package provides options for maintaining visual selection when
toggling modes. To enable this, set `persist_visual`,
`persist_visual_to_evil`, and `persist_visual_to_god` to true.


### Cursor Appearance


Customize cursor appearance for different states. For example:

    (setq evil-god-state-cursor '(box "Red"))
    (setq evil-insert-state-cursor '(bar "Red"))


## Best Practices

If you\'re well-acquainted with both Vim and Emacs keybindings, this
package helps you get the best of both worlds. Customize the keybindings
and other options to fit seamlessly into your workflow.
