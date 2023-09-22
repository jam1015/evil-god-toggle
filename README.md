# evil-god-toggle

A package to seamlessly toggle between
[evil-mode](https://github.com/emacs-evil/evil) and
[god-mode](https://github.com/chrisdone/god-mode).

Inspired by and a heavily modified version of
[evil-god-state](https://github.com/gridaphobe/evil-god-state)

## Background

Evil Mode provides very accurate Vim emulation in emacs.

God mode (in my interpretation) introduces a kind of modal editing in Emacs where non-insertion text editing capabilities are executed without modifier keys, analogous to insert mode in Vim, and the Fundamental state of Emacs is analogous to Insert Mode.

I wanted a good way to switch between Evil and God Mode. Evil-God-State does this, making God Mode a mode contained withing Evil Mode, but switches back to Evil mode after one command.  This package makes the change to God-mode persistent, providing functions to toggle between evil-mode and god mode.

## Installation

Use straignt.el to install from GitHub, or clone this repository and
manually add it to your load-path. 

- **TODO**: Get this package on MELPA if it gets good reception.

## Usage

The two functions that the user interacts with are:

### `god-toggle`

It takes the argument `append`.  

- If Emacs is in Normal mode and this is called, Emacs will enter God mode and the cursor will remain in the same place. 

- If Emacs is in God mode and this function is called, Emacs will enter Evil's insert mode. Further, if the argument `append` is non-nill the cursur will move one space forward as if `a` was pressed to insert Normal mode in Vim. 

- If Emacs is in Evil's insert mode and this function is called, Emacs will go into the God Mode sub-mode of evil defined by this package.  Further, the motion of the cursor is determined by the global variable `insert-to-god-cursor-strategy`:

    - `default` (or any value different from the next three options): Same cursor motion as switching from Insert mode to Normal mode in Vim. The cursor is moved one space back.  Takes no dependence on the value of the argument `append`. Repeatedly calling `god-toggle` with `append` nil will move the cursor backwards each cycle.  Repeatedly calling `god toggle` with `append` non-nil will move the cursor forwards and backwards in the same two position.s

     - `same`: If `append` is `nil` then the cursor remains in the same place. If `append` is non-`nil` then the cursor moves back one space. This means that repeatedtly calling the 

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
