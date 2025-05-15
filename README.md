# evil-god-toggle
![GPLv3](https://img.shields.io/badge/license-GPL--3.0-blue)

- [Description](#description)
- [TL;DR](#tldr)
- [Motivation](#motivation)
- [New Emacs Minor Mode Introduced](#new-emacs-minor-mode-introduced)
- [New Evil States Introduced](#new-evil-states-introduced)
- [User-facing Functions](#user-facing-functions)
- [Customizations](#customizations)
- [Depends-on](#depends-on)
- [Installation / Configuration](#installation--configuration)
- [License](#license)
- [TODO](#todo)
- [Contributing](#contributing)


## Description

`evil-god-toggle` is an Emacs package that seamlessly toggles between
[`evil`](https://github.com/emacs-evil/evil) (Vim emulation) and
[`god-mode`](https://github.com/emacsorphanage/god-mode) (Emacs modal editing).
 `god-mode` becomes a state of Evil just like any other state (insert, visual, etc)

## TLDR

[Here](https://github.com/jam1015/evil-god-toggle?tab=readme-ov-file#installationconfiguration)
is an example installation and configuration.

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

### `god-mode`

Emacs should function just like if you were in god mode, but it is now a state
of Evil mode on the same level with Normal mode and insert mode


### `god-off-mode`

Basically an Evil mode with no keybindings, very similar to the 'Emacs Mode' of
Evil that you reach with `C-z`.  This is included as a clean state that is easy
to switch back and forth between with `god-mode`, whereas `Emacs Mode` has
previously presented challenges regarding state transitions to the author of
this plugin.


### `god-once-mode`

Execute a single command in god mode and then switch back to the previous mode
of Evil.


## User-facing Functions


### `evil-god-toggle-god-toggle`

**Arguments:** None (interactive)

**Return Value:** `nil`

**Description:** If currently in God state, calls
`evil-god-toggle-execute-in-god-off-state` (enter a transient "God‑off"
state). Otherwise, calls `evil-god-toggle-execute-in-god-state` (enter
full God state). Protects against invocation from the minibuffer.

If `evil-god-toggle-persist-visual` (see documentation for more on that) specifies it, a Evil mode visual selection will be preserved as an active region in god mode.

**Intended Purpose:** A single entry point for users to flip between
Evil and God modes with one key. Handles both toggling on (to God) and
toggling off (to `god-off-state`)


### `evil-god-toggle-stop-choose-state`

**Arguments:** `alternate_target` (symbol) -- one of `'normal`,
`'insert`, or `'visual`.

**Return Value:** `nil`

**Description:** Called when exiting God mode. If there is an active
region and `evil-god-toggle-persist-visual` allows it, stashes the
region bounds and direction, then switches into Evil visual state.
Otherwise, switches into the specified `alternate_target` Evil state.

**Intended Purpose:** Centralize the logic for "where should I land in
Evil when I leave God?", handling visual‑selection persistence or
falling back to normal/insert states.

### `evil-god-toggle-once`

**Arguments:** None (interactive)

**Return Value:** `nil`

**Description:** Enters a one‑shot God state ("god-once"). Saves
`last-command` and installs both `pre-command-hook` and
`post-command-hook` so that after exactly one non‑prefix command Emacs
exits back to the previous Evil state. Signals an error if already in
any God state or in the minibuffer.

**Intended Purpose:** Give users quick access to God‑mode keybindings
for a single command (e.g. one control‑key chord) without having to
remember to toggle back themselves.

### `evil-god-toggle-bail`

**Arguments:** None (interactive)

**Return Value:** `nil`

**Description:** Immediately removes any transient God hooks, disables
God mode, and forces Evil normal state.

**Intended Purpose:** Exit God mode.

### Customizations

All of the following variables live in the `evil-god-toggle`
customization group.

### `evil-god-toggle-persist-visual`

-   **Type:**
    `(choice (const :tag "Always" always) (const :tag "To God" to-god) (const :tag "To Evil" to-evil) (const :tag "Never" nil))`
-   **Default:** `always`
-   **Description:** Controls whether and when an existing visual
    selection or active region is preserved when toggling between Evil and God modes.
    -   `always` -- keep the region on both entry and exit.
    -   `to-god` -- preserve only when entering God.
    -   `to-evil` -- preserve only when returning to Evil.
    -   `nil` -- never preserve the region.

    *Note*: This only works with characterwise visual selection/active regions, not linewise selections or blockwise.

### `evil-god-toggle-global`

-   **Type:** `boolean`
-   **Default:** `nil`
-   **Description:** When non‑nil, toggling God mode uses
    `god-mode-all` (affecting all buffers) instead of the
    buffer‑local `god-local-mode`.


## Depends-On

Depends on installation of [evil](https://github.com/emacs-evil/evil) and [god-mode](https://github.com/emacsorphanage/god-mode).


## Installation and Configuration


### Example Use-package (with elpaca integration)

The author of this plugin uses [Elpaca](https://github.com/progfolio/elpaca/) to manage packages although [Straight](https://github.com/radian-software/straight.el) might be easier to use.  There is also emacs native package management [package.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/package.el).  See the documentation of those package managers / other sources for how to integrate with the [use-package](https://github.com/jwiegley/use-package) macro. [More documentation for use-package](https://www.gnu.org/software/emacs/manual/html_node/use-package/).

Here is an example that works with Elpaca's use-package integration:

```el
(use-package evil-god-toggle
  :ensure (:host github :repo "jam1015/evil-god-toggle")
  :after (evil god-mode which-key)

  ;;—————— Initialization ——————
  :init
  ;; (Optionally tweak any defcustoms here, before the package loads.)
  ;; For example, to set visual-selection persistence or global mode:
  ;; (setq evil-god-toggle-persist-visual 'always
  ;;       evil-god-toggle-global        t)

  ;;—————— Configuration ——————
  :config
  ;; 1) Turn on the minor mode so its keymap & lighter are active:
  (evil-god-toggle-mode 1)

  ;; 2) Toggle into “full” God mode from Normal/Insert:
  (evil-define-key '(normal insert)
    evil-god-toggle-mode-map
    ;; C-; → enter persistent god-mode
    (kbd "C-;") #'evil-god-toggle-execute-in-god-state)

  ;; 3) From within God mode, go back to the previous Evil state:
  (evil-define-key 'god
    evil-god-toggle-mode-map
    ;; C-; → change back to whatever state was active before
    (kbd "C-;") #'evil-change-to-previous-state)

  ;; 4) Bind Escape in both god & god-off states to return to Normal:
  (evil-define-key '(god god-off)
    evil-god-toggle-mode-map
    [escape] (lambda ()
               (interactive)
               ;; On exit, land in ‘normal’ state (you can swap to 'insert if you prefer)
               (evil-god-toggle-stop-choose-state 'normal)))

  ;; 5) A “flip-flop” binding: M-; toggles on/off God mode in any state
  (evil-define-key '(god god-off normal insert)
    evil-god-toggle-mode-map
    (kbd "M-;") #'evil-god-toggle-god-toggle)

  ;; 6) One-shot God: press C-, in Normal/Insert to enter for exactly one command
  (evil-define-key '(normal insert)
    evil-god-toggle-mode-map
    (kbd "C-,") #'evil-god-toggle-once)

  ;;—————— Custom options ——————
  ;; Preserve any active region when toggling modes:
  (setq evil-god-toggle-persist-visual 'always
        ;; Make god-mode global (applies to all buffers) instead of buffer-local:
        evil-god-toggle-global        t)

  ;;—————— Optional cursor styling ——————
  ;; (setq evil-god-state-cursor     '(box    "Red")
  ;;       evil-god-off-state-cursor '(bar    "Green")
  ;;       evil-insert-state-cursor '(bar    "Red")
  ;;       evil-visual-state-cursor '(hollow "Red")
  ;;       evil-normal-state-cursor '(hollow "Black"))
  )
```



## Version/License

- **Version:** 0.1.0
- **Author:** [Jordan Mandel](https://github.com/jam1015/)
- **License:** GPL-3.0-or-later


## TODO

- Implement proper testing
- linewise/blockwise visual selection/active region preservation.  Attempted this but it was complicated.

## Contributing

Please feel free to complain by opening an issue or be helpful by opening a
pull request.

## Related


[evil-collection]:    https://github.com/emacs-evil/evil-collection
[god-mode]:           https://github.com/emacsorphanage/god-mode
[evil-god-state]:     https://github.com/gridaphobe/evil-god-state
[installation]:       #installation--configuration
