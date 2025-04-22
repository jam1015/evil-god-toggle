# evil-god-toggle

**Version:** 0.1.0\ **Author:** Jordan Mandel (\<jam1015 on GitHub\>)\
**License:** GPL-3.0-or-later


## Description

`evil-god-toggle` is an Emacs package that seamlessly toggles between
[`evil`](https://github.com/emacs-evil/evil) (Vim emulation) and
[`god-mode`](https://github.com/emacsorphanage/god-mode) (Emacs modal editing).
 `god-mode` becomes a submode of Evil on the same level as any other mode (insert, visual, etc)


## Motivation

As a Neovim user I was curious about Emacs.  However, key-chords were difficult
to get used to, Vim bindings are very habitual, and I heard that even
evil+[evil-collection](https://github.com/emacs-evil/evil-collection) didn't
cover every corner case of Emacs use.

`god-mode` seemed like a great and simple way to add modality to emacs; it
makes it so that it is as if the control key is always pressed down, so that
the command bound to control+[some other key] is invoked when that other-key is
pressed alone, without control, with other easy ways to invoke other modifiers.
In my opinion this makes `god-mode` is analoglous Normal mode in Vim, where
keys are bound to commands, and the normal state of Emacs (`god-mode` not
turned on) is like insert mode in Vim, where you can press key-chords to
operate on text, but otherwise you can type normally.

[This section](https://github.com/emacsorphanage/god-mode?tab=readme-ov-file#usage-with-evil)
of the `god-mode` README says lists ways of using god-mode alongside evil.
None of them make a way of persistently using `god-mode` alongside evil to my
satisfaction.

The third bullet in that section point goes over how
[`evil-god-state`](https://github.com/gridaphobe/evil-god-state) creates a
state of evil-mode dedicated to god-mode that lasts for one command.  This
package extends that by creating a persistent state of Evil dedicated completely
to `god-mode`, while also including a one-off god state similar to `evil-god-state`


## New Emacs Minor Mode Introduced
- `evil-god-toggle-mode` 
 Mode that contains the map to which the user should assign all keymaps to commands introduced in this package.

## New Evil Modes Introduced

### `god-mode`

Emacs should function just like if you were in god mode, but it is now a state of Evil mode on the same level with Normal mode and insert mode


### `god-off-mode`

Basically an Evil mode with no keybindings, very similar to the 'Emacs Mode' of Evil that you reach with `C-z`.  This is included as a clean state that is easy to switch back and forth between with `god-mode`, whereas `Emacs Mode` has its own rules for when it is switched in and out of.

### `god-once-mode`

Execute a single command in god mode and then switch back to the previous mode of Evil.


## User-facing Functions


### `evil-god-toggle--god`

**Arguments:** None (interactive)

**Return Value:** `nil`

**Description:** If currently in God state, calls
`evil-god-toggle--execute-in-god-off-state` (enter a transient "God‑off"
state). Otherwise, calls `evil-god-toggle--execute-in-god-state` (enter
full God state). Protects against invocation from the minibuffer.

If `evil-god-toggle-persist-visual` (see documentation for more on that) specifies it, a Evil mode visual selection will be preserved as an active region in god mode.

**Intended Purpose:** A single entry point for users to flip between
Evil and God modes with one key. Handles both toggling on (to God) and
toggling off (to `god-off-state`)




### `evil-god-toggle--stop-choose-state`

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

## evil-god-toggle--once

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

## evil-god-toggle--bail

**Arguments:** None (interactive)

**Return Value:** `nil`

**Description:** Immediately removes any transient God hooks, disables
God mode, and forces Evil normal state. Displays a "\-- Aborted God mode
\--" message.

**Intended Purpose:** Provide an "emergency exit" so that if the user
finds themselves stuck in God mode they can instantly return to standard
Evil behavior.


## Customizations

All of the following variables live in the `evil-god-toggle`
customization group.

### `evil-god-toggle-persist-visual`

-   **Type:**
    `(choice (const :tag "Always" always) (const :tag "To God" to-god) (const :tag "To Evil" to-evil) (const :tag "Never" nil))`
-   **Default:** `always`
-   **Description:** Controls whether and when an existing visual
    selection is preserved when toggling between Evil and God modes.
    -   `always` -- keep the region on both entry and exit.
    -   `to-god` -- preserve only when entering God.
    -   `to-evil` -- preserve only when returning to Evil.
    -   `nil` -- never preserve the region.

### `evil-god-toggle-global`

-   **Type:** `boolean`
-   **Default:** `nil`
-   **Description:** When non‑nil, toggling God mode uses
    `god-mode-all` (affecting all buffers) instead of the
    buffer‑local `god-local-mode`.



## Installation


### Use-package (with Elpaca)

```el (use-package evil-god-toggle :ensure (:after evil :host github :repo
"jam1015/evil-god-toggle") :config (global-set-key (kbd "C-;")
#'evil-god-toggle) ;; preserve visual selection when toggling to God (setq
evil-god-toggle-persist-visual-to-god t) ;; persist both directions (setq
evil-god-toggle-persist-visual t) ;; use global God mode instead of per-buffer
(setq evil-god-toggle-global t)

;; define God-state keybindings (suggested) (evil-define-key 'god global-map
"C-;" #'evil-god-toggle) (evil-define-key 'god global-map [escape]
#'evil-god-toggle-stop-choose-state) (evil-define-key '(normal) global-map ","
#'evil-god-toggle-once) (evil-define-key 'god global-map (kbd "")
#'evil-god-toggle-bail)

;; customize cursors (setq evil-god-state-cursor '(box   "Red")) (setq
evil-insert-state-cursor '(bar   "Red")) (setq evil-visual-state-cursor
'(hollow "Red")) (setq evil-normal-state-cursor '(hollow "Black"))) ```

## Customization Options

  Variable                                   Default   Description
  ------------------------------------------ ---------
  -----------------------------------------------------------------------------------
  `evil-god-toggle-persist-visual`           `nil`     If non-nil, persist
  visual selection both to and from God mode.
  `evil-god-toggle-persist-visual-to-god`    `nil`     If non-nil, persist
  visual selection when entering God mode.
  `evil-god-toggle-persist-visual-to-evil`   `nil`     If non-nil, persist
  visual selection when returning to Evil mode. `evil-god-toggle-global`
  `nil`     If non-nil, use `god-mode-all` (global) instead of `god-local-mode`
  (per-buffer).

## Commands

  Command                               Suggested Keybinding
  Description -------------------------------------
--------------------------------
--------------------------------------------------------------------------------------------------------------
`evil-god-toggle`                     [C-;]{.kbd}                      Toggle
between Evil and God modes (default to Insert state on return).
`evil-god-toggle-stop-choose-state`   [ESC]{.kbd} in God state         Return
from God to Evil; if visual region active and persistence enabled, restore
visual, else Normal state. `evil-god-toggle-once`                [,]{.kbd} in
Normal state        Enter God mode for next command only, then revert to Evil.
`evil-god-toggle-bail`                [Shift+ESC]{.kbd} in God state   Abort
God mode immediately, return to Evil Normal.

## Function Reference

### `evil-god-toggle`

Public: toggle between insert mode and god mode.

### `evil-god-toggle-bail`

Public: immediate exit from God mode to Evil Normal, cleaning up hooks.

### `evil-god-toggle-stop-choose-state`

Public: choose Evil state to return to, stashing/restoring visual selection if
enabled.

### `evil-god-toggle-once`

Public: momentary God state for a single command, then revert.

### `evil-god-toggle-start-hook-fun`

Runs on entering God state: removes Evil visual hooks, enables God (global or
local).

### `evil-god-toggle-stop-hook-fun`

Runs on exiting God state: restores Evil visual hooks, disables God (global or
local).

### `evil-god-toggle-execute-in-god-state`

Internal: prepare last-command, optionally stash region, then enter God state.

### `evil-god-toggle-fix-last-command`

Internal: restores `last-command` for seamless Evil command dispatch.

### `evil-god-toggle-stop-execute-in-god-state`

Internal: leaves God state then calls transition based on `TARGET`.

### `evil-god-toggle-transition-to-normal`, `-to-insert`, `-to-visual`

Perform appropriate Evil state switch, handling region activation/deactivation.

### `evil-god-toggle--exit-once`

Internal: triggers exit after the one-shot God command runs.


## Variables

  Variable                                   Description
  ------------------------------------------
  ------------------------------------------------------
  `evil-god-toggle--visual-beg`              Stashed region start for visual
  restore. `evil-god-toggle--visual-end`              Stashed region end
  (inclusive) for visual restore. `evil-god-toggle--visual-forward`
  Whether stashed region was forward. `evil-god-toggle-last-command`
  Last command before entering God state.
  `evil-god-toggle-ran-first-evil-command`   Tracks if the first Evil command
  ran after toggling.

## Hooks and Integration

-   **Starts:** `evil-god-toggle-start-hook-fun` on `:entry-hook` of Evil God
state.
-   **Stops:** `evil-god-toggle-stop-hook-fun` on `:exit-hook` of Evil God
state.
-   `evil-visual-state-entry-hook`:
`evil-god-toggle-check-and-update-previous-state-visual` ensures correct
`evil-previous-state`.

## Customization Group

All options are under `Customization > evil-god-toggle`.

## License

Licensed under the GNU General Public License v3.0 or later.

## Contributing

Please feel free to complain by opening an issue or be helpful by opening a
pull request.
