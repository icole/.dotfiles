# .dotfiles

Common config files to be linked when running `stow -d ~/.dotfiles .`

## Apply on a fresh machine

```sh
# 1. install GNU Stow  (Fedora/Omadora: dnf, Arch/Omarchy: pacman)
sudo dnf install stow        # or: sudo pacman -S stow

# 2. clone and link
git clone git@github.com:icole/.dotfiles.git ~/.dotfiles
cd ~/.dotfiles
stow .
```

### Onto an existing Omadora install (files already present)

Omadora pre-creates `~/.config/hypr/*.conf`, `~/.config/waybar/*`, etc., so a plain
`stow .` will report conflicts. Use stow's *adopt* trick to take them over:

```sh
cd ~/.dotfiles
stow --adopt .   # links everything; moves any pre-existing files into the repo
git restore .    # discard the adopted copies, keeping THIS repo's versions
```

After that, every linked file is a symlink pointing at this repo, so edits on any
machine are saved with a normal `git commit`.

## Omadora / Hyprland tweaks

Stow-linked overrides (layer on top of Omadora's defaults in `~/.local/share/omadora`):

- **`.config/hypr/bindings.conf`** — vim group-tab switching (`Alt+Shift+H/L`), resize
  submap (`Super+R` then `hjkl`), direct app search (`Super+A`), maximize-with-decorations
  (`Super+Shift+F`).
- **`.config/hypr/looknfeel.conf`** — rounded corners, inactive dim/transparency, frostier
  blur, deeper shadows, workspace slide animation, 2px borders.
- **`.config/hypr/input.conf`** — natural scroll (trackpad + mouse), two-finger right-click,
  3-finger horizontal workspace swipe.
- **`.config/waybar/{config.jsonc,style.css}`** — always-visible system tray (no drawer),
  status glyphs at 18px / tray icons at 14px.
- **`.config/autostart/org.signal.Signal.desktop`** — start Signal minimized to the tray on
  login (requires enabling Signal's in-app "minimize to system tray" setting once).

### Per-machine (NOT auto-linked)

- **`.config/hypr/monitors.conf.example`** — display scaling is machine-specific. Copy it and
  edit per machine:
  ```sh
  cp ~/.dotfiles/.config/hypr/monitors.conf.example ~/.config/hypr/monitors.conf
  ```
  Key lesson baked in: on a non-HiDPI panel use `env = GDK_SCALE,1` and set the *Hyprland*
  monitor `scale` (e.g. `1.25`) — do **not** stack `GDK_SCALE` on top of a monitor scale or
  GTK apps (Firefox) get double-scaled.

### Manual tweaks not tracked here

- **mako notification spacing** (`outer-margin=8`): lives in the active Omadora *theme* file
  (`~/.config/omadora/current/theme/mako.ini`), so it's theme-coupled. Re-add after install:
  add `outer-margin=8` under the `include=` line, then `makoctl reload`.
- **Antigravity icons / .desktop**: app-install specific (hardcoded `~/opt` paths), set up
  per machine.
