# evil-god-toggle

**Version:** 0.1\
**Author:** Jordan Mandel (\<jam1015 on GitHub\>)\
**License:** GPL-3.0-or-later

## Description

`evil-god-toggle` is an Emacs package that seamlessly toggles between
`evil-mode` (Vim emulation) and `god-mode` (Emacs modal editing). It
preserves visual selections optionally, supports both per-buffer and
global God mode, and provides one-shot or persistent toggles.

## Installation


### Use-package (with Elpaca)

```el
(use-package evil-god-toggle
  :ensure (:after evil :host github :repo "jam1015/evil-god-toggle")
  :config
  (global-set-key (kbd "C-;") #'evil-god-toggle)
  ;; preserve visual selection when toggling to God
  (setq evil-god-toggle-persist-visual-to-god t)
  ;; persist both directions
  (setq evil-god-toggle-persist-visual t)
  ;; use global God mode instead of per-buffer
  (setq evil-god-toggle-global t)

  ;; define God-state keybindings (suggested)
  (evil-define-key 'god global-map "C-;" #'evil-god-toggle)
  (evil-define-key 'god global-map [escape] #'evil-god-toggle-stop-choose-state)
  (evil-define-key '(normal) global-map "," #'evil-god-toggle-once)
  (evil-define-key 'god global-map (kbd "") #'evil-god-toggle-bail)

  ;; customize cursors
  (setq evil-god-state-cursor '(box   "Red"))
  (setq evil-insert-state-cursor '(bar   "Red"))
  (setq evil-visual-state-cursor '(hollow "Red"))
  (setq evil-normal-state-cursor '(hollow "Black")))
```

## Customization Options

  Variable                                   Default   Description
  ------------------------------------------ --------- -----------------------------------------------------------------------------------
  `evil-god-toggle-persist-visual`           `nil`     If non-nil, persist visual selection both to and from God mode.
  `evil-god-toggle-persist-visual-to-god`    `nil`     If non-nil, persist visual selection when entering God mode.
  `evil-god-toggle-persist-visual-to-evil`   `nil`     If non-nil, persist visual selection when returning to Evil mode.
  `evil-god-toggle-global`                   `nil`     If non-nil, use `god-mode-all` (global) instead of `god-local-mode` (per-buffer).

## Commands

  Command                               Suggested Keybinding                       Description
  ------------------------------------- -------------------------------- --------------------------------------------------------------------------------------------------------------
  `evil-god-toggle`                     [C-;]{.kbd}                      Toggle between Evil and God modes (default to Insert state on return).
  `evil-god-toggle-stop-choose-state`   [ESC]{.kbd} in God state         Return from God to Evil; if visual region active and persistence enabled, restore visual, else Normal state.
  `evil-god-toggle-once`                [,]{.kbd} in Normal state        Enter God mode for next command only, then revert to Evil.
  `evil-god-toggle-bail`                [Shift+ESC]{.kbd} in God state   Abort God mode immediately, return to Evil Normal.

## Function Reference

### `evil-god-toggle-start-hook-fun`

Runs on entering God state: removes Evil visual hooks, enables God
(global or local).

### `evil-god-toggle-stop-hook-fun`

Runs on exiting God state: restores Evil visual hooks, disables God
(global or local).

### `evil-god-toggle-execute-in-god-state`

Internal: prepare last-command, optionally stash region, then enter God
state.

### `evil-god-toggle-fix-last-command`

Internal: restores `last-command` for seamless Evil command dispatch.

### `evil-god-toggle-stop-execute-in-god-state`

Internal: leaves God state then calls transition based on `TARGET`.

### `evil-god-toggle-transition-to-normal`, `-to-insert`, `-to-visual`

Perform appropriate Evil state switch, handling region
activation/deactivation.

### `evil-god-toggle-stop-choose-state`

Public: choose Evil state to return to, stashing/restoring visual
selection if enabled.

### `evil-god-toggle-once`

Public: momentary God state for a single command, then revert.

### `evil-god-toggle--exit-once`

Internal: triggers exit after the one-shot God command runs.

### `evil-god-toggle-bail`

Public: immediate exit from God mode to Evil Normal, cleaning up hooks.

## Variables

  Variable                                   Description
  ------------------------------------------ ------------------------------------------------------
  `evil-god-toggle--visual-beg`              Stashed region start for visual restore.
  `evil-god-toggle--visual-end`              Stashed region end (inclusive) for visual restore.
  `evil-god-toggle--visual-forward`          Whether stashed region was forward.
  `evil-god-toggle-last-command`             Last command before entering God state.
  `evil-god-toggle-ran-first-evil-command`   Tracks if the first Evil command ran after toggling.

## Hooks and Integration

-   **Starts:** `evil-god-toggle-start-hook-fun` on `:entry-hook` of
    Evil God state.
-   **Stops:** `evil-god-toggle-stop-hook-fun` on `:exit-hook` of Evil
    God state.
-   `evil-visual-state-entry-hook`:
    `evil-god-toggle-check-and-update-previous-state-visual` ensures
    correct `evil-previous-state`.

## Customization Group

All options are under `Customization > evil-god-toggle`.

## License

Licensed under the GNU General Public License v3.0 or later.

## Contributing

Please feel free to complain by opening an issue or be helpful by opening a pull request.
