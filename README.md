# evil-god-toggle

A package to seamlessly toggle between
[evil-mode](https://github.com/emacs-evil/evil) and
[god-mode](https://github.com/chrisdone/god-mode).

**Inspired by and a heavily modified version of
[evil-god-state](https://github.com/gridaphobe/evil-god-state)**

## Background


After learning [Neo]vim fairly well, I wanted to give Emacs a shot; what attracts me is that it represents an old way of computer interaction, hearkening back to LISP machines, and emulation of a computer where all programs seamlessly interacted with each other using a shared user interface that integrates better than various GUI programs or clumsily piping text from one UNIX command to the next. 

## Installation

Use straignt.el to install from GitHub, or clone this repository and
manually add it to your load-path.

## Usage

The default keybindings are:

-   `C-;`: Toggle to god-mode
-   `C-,`: Toggle back to evil-mode

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
