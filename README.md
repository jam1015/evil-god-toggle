# evil-god-toggle

[![GPLv3](https://img.shields.io/badge/license-GPL--3.0-blue)](https://www.gnu.org/licenses/gpl-3.0.en.html#license-text)
[![MELPA](https://melpa.org/packages/evil-god-toggle-badge.svg)](https://melpa.org/#/evil-god-toggle)
[![MELPA Stable](https://stable.melpa.org/packages/evil-god-toggle-badge.svg)](https://stable.melpa.org/#/evil-god-toggle)

- [Description](#description)
- [TL;DR](#tldr)
- [Motivation](#motivation)
- [New Emacs Minor Mode Introduced](#new-emacs-minor-mode-introduced)
- [New Evil States Introduced](#new-evil-states-introduced)
- [User-facing Functions](#user-facing-functions)
- [Customizations](#customizations)
- [Depends-on](#depends-on)
- [Installation and Configuration](#installation-and-configuration)
- [Version/License](#versionlicense)
- [TODO](#todo)
- [Contributing](#contributing)


## Description

`evil-god-toggle` is an Emacs package that seamlessly toggles between
[`evil`](https://github.com/emacs-evil/evil) (Vim emulation) and
[`god-mode`](https://github.com/emacsorphanage/god-mode) (Emacs modal editing).
 `god-mode` becomes a state of Evil just like any other state (insert, visual, etc)

## TLDR

[Here](#installation-and-configuration) is an example installation and configuration.

## Motivation

As a Neovim user I was curious about Emacs. However, key-chords were difficult
to get used to, Vim bindings are very habitual, and I heard that even
evil+[evil-collection](https://github.com/emacs-evil/evil-collection) didn't cover every corner case of Emacs use.

`god-mode` seemed like a great and simple way to add modality to emacs; it
makes it so that it is as if the control key is always pressed down, so that
the command bound to control+[some other key] is invoked when that other-key is
pressed alone, without control, with other easy ways to invoke other modifiers.
In my opinion  `god-mode` is analogous Normal mode in Vim, where
keys are bound to commands, and the normal state of Emacs (`god-mode` not
turned on) is like insert mode in Vim, where you can press key-chords to
operate on text, but otherwise you can type normally.

[This section](https://github.com/emacsorphanage/god-mode?tab=readme-ov-file#usage-with-evil)
of the `god-mode` README says lists ways of using god-mode alongside evil. None
of them make a way of persistently using `god-mode` alongside evil to my
satisfaction.

The third bullet in that section point goes over how
[`evil-god-state`](https://github.com/gridaphobe/evil-god-state) creates a
state of evil-mode dedicated to god-mode that lasts for one command.  This
package extends that by creating a persistent state of Evil dedicated completely
to `god-mode`, while also including a one-off god state similar to `evil-god-state`.


## New Emacs Minor Mode Introduced

`evil-god-toggle-mode`

Mode that contains the map to which the user should assign all keymaps to commands introduced in this package.

## New Evil States Introduced

### `god`

Emacs should function just like if you were in god mode, but it is now a state
of Evil on the same level with Normal and Insert.


### `god-off`

Basically an Evil state with no keybindings, very similar to the `Emacs State` of
Evil that you reach with `C-z`.  This is included as a clean state that is easy
to switch back and forth between from `god`, whereas `Emacs State` has
previously presented challenges regarding state transitions to the author of
this plugin. Useful for text insertion.


### `god-once`

Execute a single command in god mode and then switch back to the previous state
of Evil.




## User-facing Functions

### `evil-god-toggle-stop-god-state-maybe-visual`

**Arguments:** `alternate-target` *(symbol: `'normal`, `'insert`, or
`'visual`)*

**Return Value:** `nil`

**Description:** Exits God and chooses the destination Evil state.
If a characterwise region is active and `evil-god-toggle-persist-visual`
is `always` or `to-evil`, it stashes the region bounds/orientation and
restores visual selection; otherwise it falls back to the specified
`alternate-target`. Protects against minibuffer invocation.

**Intended Purpose:** The recommended exit entry point for keybindings
(e.g. [Escape]{.kbd}), handling visual persistence and landing in the
correct Evil state.

**Respects `evil-god-toggle-persist-visual`**.

### `evil-god-toggle-god-toggle`

**Arguments:** None (interactive)

**Return Value:** `nil`

**Description:** If invoked from the minibuffer, signals a user error.
Otherwise, toggles God on or off: if currently in God state, calls
`evil-god-toggle-execute-in-god-off-state`; otherwise calls
`evil-god-toggle-execute-in-god-state`. Preserves any active visual
region according to `evil-god-toggle-persist-visual`.

**Intended Purpose:** A single flip-flop entry point to switch
seamlessly between Evil and God.
Should be bound to `god` and `god-off` states.

### `evil-god-toggle-execute-in-god-state`

**Arguments:** None (interactive)

**Return Value:** `nil`

**Description:** Saves the current `last-command`, adds a transient
hook (to restore `last-command` ), enables God (buffer-local), and 
enters `evil-god-state`. Optionally restores a characterwise
visual region on entry if `evil-god-toggle-persist-visual` permits.

**Intended Purpose:** Enter persistent God state from any Evil state,
with optional visual-selection persistence.

### `evil-god-toggle-execute-in-god-off-state`

**Arguments:** None (interactive)

**Return Value:** `nil`

**Description:** Saves the current `last-command`, adds transient hooks,
disables God while entering the `god-off` state, and then
optionally restores any active visual region according to
`evil-god-toggle-persist-visual`.

**Intended Purpose:** Provide a clean "God-off" state (akin to Emacs
state) that can be toggled back into God.

### `evil-god-toggle-stop-execute-in-god-state`

**Arguments:** `target` *(symbol: `'normal`, `'insert`, or `'visual`)*

**Return Value:** `nil`

**Description:** Internal wrapper that exits the current God or God-off
state and transitions to the specified Evil state. It deactivates any
active region, invokes the matching transition function
(`evil-normal-state`, `evil-insert-state` or a visual restore), and
updates the mode line.

**Intended Purpose:** Used by keybindings and other functions to perform
the actual state switch when leaving God.

***Does not respect `evil-god-toggle-persist-visual`***.
***Better to use `evil-god-toggle-stop-god-state-maybe-visual`***


### `evil-god-toggle-once`

**Arguments:** None (interactive)

**Return Value:** `nil` (or error if already in God state or minibuffer)

**Description:** Enters a temporary God state for exactly one non-prefix
command. Saves `last-command`, installs `pre-command-hook` and
`post-command-hook` to restore the previous Evil state after that
command, and enables God. Signals an error if already in `god` or
`god-once` or if invoked from the minibuffer.

**Intended Purpose:** Provide quick, one-shot access to God without
needing to toggle back manually.



### `evil-god-toggle-bail`

**Arguments:** None (interactive)

**Return Value:** `nil`

**Description:** Immediately aborts any God or god-once session by
removing all transient hooks, disabling God, and forcing Evil
normal state.

**Intended Purpose:** Emergency escape to ensure you return to Evil
Normal state regardless of current God toggles.

## Customizations

All of the following variables live in the `evil-god-toggle`
customization group.

### `evil-god-toggle-persist-visual`

-   **Type:**
    `(choice (const :tag "Always" always) (const :tag "To God" to-god) (const :tag "To Evil" to-evil) (const :tag "Never" nil))`
-   **Default:** `always`
-   **Description:** Controls whether and when an existing visual
    selection or active region is preserved when toggling between Evil and God states.
    -   `always` -- keep the region on both entry and exit.
    -   `to-god` -- preserve only when entering God.
    -   `to-evil` -- preserve only when returning to Evil.
    -   `nil` -- never preserve the region.

    *Note*: This only works with characterwise visual selection/active regions, not linewise selections or blockwise.



## Depends-On

Depends on installation of [evil](https://github.com/emacs-evil/evil) and [god-mode](https://github.com/emacsorphanage/god-mode).


## Installation and Configuration


### Example Use-package (with elpaca integration)

The author of this plugin uses [Elpaca](https://github.com/progfolio/elpaca/) to manage packages although [Straight](https://github.com/radian-software/straight.el) might be easier to use.  There is also emacs native package management [package.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/package.el).  See the documentation of those package managers / other sources for how to integrate with the [use-package](https://github.com/jwiegley/use-package) macro. [More documentation for use-package](https://www.gnu.org/software/emacs/manual/html_node/use-package/).

Here is an example that works with Elpaca's use-package integration:

```el
(use-package evil-god-toggle
  :ensure (:host github
           :repo "jam1015/evil-god-toggle")
  :after (evil god-mode )
  :init
  
  :config
  ;; 1. Enable the global minor mode (so its keymap + lighter are active)
  (evil-god-toggle-mode 1)

  ;; 2. Enter persistent god mode from Normal/Insert/God-off states with C-;
  (evil-define-key '(normal insert god-off)
    evil-god-toggle-mode-map
    (kbd "C-;") (lambda ()
                  (interactive)
                  (evil-god-toggle-execute-in-god-state)))

  ;; 3. Exit god mode to god-off state with C-; 
  (evil-define-key 'god
    evil-god-toggle-mode-map
    (kbd "C-;") (lambda ()
                  (interactive)
                  (evil-god-toggle-execute-in-god-off-state)))

  ;; 4. Escape from any god state returns to Normal
  (evil-define-key '(god god-off god-once)
    evil-god-toggle-mode-map
    [escape] (lambda ()
               (interactive)
               (evil-god-toggle-stop-god-state-maybe-visual 'normal)))

  ;; 5. One-shot God mode: C-, in Normal for exactly one command
  (evil-define-key 'normal
    evil-god-toggle-mode-map
    (kbd "C-,") (lambda ()
                  (interactive)
                  (evil-god-toggle-once)))

  ;; 6. Visual persistence
  (setq evil-god-toggle-persist-visual 'always))
```

## Other Notes

### Mode Hook Behavior

When `evil-god-toggle-mode` is enabled, it automatically adds a hook to
`evil-visual-state-entry-hook` that prevents getting stuck in god states
when leaving visual mode. This hook
(`evil-god-toggle--check-and-update-previous-state-visual`) ensures that
when entering visual state from `god` or `god-once`, the previous state
is reset to `normal`, so exiting visual mode returns you to normal state
rather than back to a god state.

This behavior is automatic and requires no user configuration.

### State Tag Display

Each god state displays a distinctive tag in the mode line to indicate
the current state:

-   **god state:** `" <G> "`
-   **god-once state:** `" <G (once)> "`
-   **god-off state:** `" <G (off)> "` 

These tags appear in the mode line alongside other Evil state indicators
and help you identify which god-related state is currently active.


## Version/License

- **Version:** 1.0.0
- **Author:** [Jordan Mandel](https://github.com/jam1015/)
- **License:** GPL-3.0-or-later


## TODO

- Implement proper testing
- linewise/blockwise visual selection/active region preservation.  Attempted this but it was complicated.

## Contributing

Please feel free to complain by opening an issue or be helpful by opening a
pull request.

## Related

- evil-collection:    https://github.com/emacs-evil/evil-collection
- god-mode:           https://github.com/emacsorphanage/god-mode
- evil-god-state:     https://github.com/gridaphobe/evil-god-state

AI-created plugin that changes the cursor color for different evil states. I use it in my personal config:
- cursor-contraster:  https://github.com/jam1015/cursor-contraster


