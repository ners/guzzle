# guzzle 💦

guzzle is a Wayland screen capture CLI tool.

It does not try to:
- support X11
- capture anything other than screen pixels
- have a GUI

---

## Quick start

```sh
# Screenshot an interactive selection to clipboard (default)
guzzle

# Screenshot an active window to clipboard
guzzle copy window

# Screenshot an area and save directly to a file
guzzle -f screenshot.png

# Count down 3 seconds, then copy to clipboard and also save to a file
guzzle copy window --delay 3 -f browser.png

# Record a 5-second video of a selected region
guzzle save video --duration 5

# Capture all connected monitors to individual files
guzzle save --all outputs -f 'shots/%o.png'

# Print selection geometry (slurp format) without capturing
guzzle select
```

---

## Requirements

guzzle requires the following utilities on `PATH`:

- [`slurp`](https://github.com/emersion/slurp) for interactive screen selection (the original inspiration for this project's name)
- [`grim`](https://gitlab.freedesktop.org/emersion/grim) for screenshot capture
- [`wf-recorder`](https://github.com/ammen99/wf-recorder) for video recording
- [`wl-copy`](https://github.com/bugaevc/wl-clipboard) for clipboard support
- [`notify-send`](https://gitlab.gnome.org/GNOME/libnotify) for desktop notifications (optional)

If you use **Nix**, all runtime dependencies are packaged automatically.

Arguments and flags can be passed in any order.

---

## Command reference

```sh
guzzle [verbosity] [sink] [selection] [capture] [options...]
guzzle [verbosity] select [selection] [options...]
```

---

### Sinks

A sink determines what to do with captured pixels:

| Sink | Description |
|---|---|
| `copy` | Copy to clipboard (**default**). If multiple items are captured, saves them and copies their file URIs (`text/uri-list`). |
| `save` | Save to file(s) (**default** if `-f`/`--file` is specified). Default filename: `guzzle-%d.<ext>`. |
| `print` | Stream raw data to `stdout`. Cannot stream more than one video. |

#### Sink options

- `-f, --file TEMPLATE`: Save content to `TEMPLATE`. Supports placeholders (see [Templates](#template-placeholders)). Also saves to disk when used with `copy` or `print`.
- `--no-notify`: Disable desktop notifications sent on capture completion.

---

### Selections

Selection targets determine what region on screen to capture:

| Selection | Alias | Description |
|---|---|---|
| `anything` | | Interactively pick a window, output, or drawn region (**default**) |
| `area` | `areas` | Select or draw a rectangular region |
| `window` | `windows` | Select a visible window |
| `output` | `outputs` | Select a visible monitor / display output |
| `screen` | `screens` | Full region covering all visible outputs |

Multiple selections can be specified together (e.g. `guzzle window output`) to offer candidates from all chosen kinds.

#### Selection options

- `--all`: Select every candidate of the chosen kind without interactive prompting.
- `--area-name NAME`: Retrieve a previously saved area, or save the newly drawn area as `NAME` for future runs.
- `--last-area`: Reuse the most recently selected area without prompting.
- `--no-history`: Do not store or recall saved areas, and disable history features.

*Note: `--all` and `--no-history` cannot be combined with `--area-name` or `--last-area`.*

---

### Capture modes

| Mode | Description |
|---|---|
| `screenshot` | Capture an image (**default**) |
| `video` | Record a screen video |

#### General capture options

- `--delay T`: Delay capture by `T` seconds (displays an interactive countdown).
- `--format FORMAT`: Output format:
  - Screenshots: `png` (**default**), `jpg` / `jpeg`, `ppm`
  - Videos: `mp4` (**default**), `webm`
  - If omitted, format is inferred from the `-f`/`--file` extension when present.
- `--scale FACTOR`: Scaling factor greater than 0 (e.g. `2` or `0.5`). Screenshots scale logical pixels; videos scale native pixels.

#### Screenshot options

- `--cursor`: Include mouse pointer in screenshot.
- `--quality N`: JPEG compression quality (`0`–`100`, default: `80`). Only applies when format is JPEG.

#### Video options

- `--duration T`: Record video for `T` seconds (default: `3`).
- `--framerate FPS`: Framerate for video recording.
- `--audio`: Record audio along with video.
- `--audio-device DEVICE`: Audio input device to record from (requires `--audio`).

---

### The `select` command

`guzzle select` queries geometry without capturing any pixels. By default, it prints geometry in `slurp` format (`X,Y WxH`) to `stdout`.

```sh
guzzle select [selection] [options...]
```

Supports all [selection options](#selection-options) (`--all`, `--area-name`, `--last-area`, `--no-history`).

#### Select formatting options

- `--format TEMPLATE`: Print custom formatted string per item instead of geometry.
- Kind-specific format overrides:
  - `--area-format TEMPLATE`
  - `--window-format TEMPLATE`
  - `--output-format TEMPLATE`
  - `--screen-format TEMPLATE`

Example: use guzzle as an [output chooser on wlroots](https://man.archlinux.org/man/xdg-desktop-portal-wlr.5#OUTPUT_CHOOSER):

```sh
guzzle select window output --no-history --window-format 'Window: %I' --output-format 'Monitor: %o'
```

---

### Template placeholders

Placeholders can be used in `--file` templates and `select --format` templates:

| Placeholder | Description | Example |
|---|---|---|
| `%n` | Name: window title, output name, or area name | `Firefox`, `eDP-1`, `my-area` |
| `%k` | Item kind (`area`, `window`, `output`, `screen`) | `window` |
| `%o` | Output name (for windows: output they are on) | `eDP-1` |
| `%a` | Window application ID or class | `org.mozilla.firefox` |
| `%p` | Window process ID (PID) | `1234` |
| `%I` | Window identifier (as used by `xdg-desktop-portal-wlr` and `lswt`) | `32` |
| `%i` | Index of item in this run (starting at 1) | `1` |
| `%d` | Current UTC timestamp (`%Y-%m-%dT%H:%M:%S`) | `2026-10-09T14:30:00` |
| `%x`, `%y` | Top-left X and Y coordinates | `1920`, `0` |
| `%w`, `%h` | Region width and height | `1920`, `1080` |
| `%%` | Literal percent sign | `%` |

#### Sanitisation rules

- **In `--file`**:
  - `/` and ASCII control characters are replaced with `_`.
  - Leading dots are removed to avoid accidental hidden files.
  - Values are truncated to 64 characters.
  - If multiple files resolve to the same path, `-1`, `-2`, etc. are automatically appended before the extension.
- **In `select --format`**:
  - Control characters are replaced with spaces.
  - Slashes and string lengths are preserved as-is.

---

### Logging

All logging goes to `stderr`.

The logging verbosity is controlled by the following options:

| Level | Option | Description |
|---|---|---|
| `Trace` | `--debug` | Print executed external commands and diagnostic traces |
| `Info` | *(default)* | Print informational messages (e.g. saved file paths) and interactive countdowns |
| `Warn` | `-q`, `--quiet` | Print only warnings and errors |

---

## Window manager support

guzzle communicates with window manager IPC APIs to resolve window and monitor geometries.

| Selection | Sway | Hyprland | niri |
|---|:---:|:---:|:---:|
| `area` | ✔ | ✔ | ✔ |
| `window` | ✔ | ✔ | ✔ |
| `output` | ✔ | ✔ | ✔ |
| `screen` | ✔ | ✔ | ✔ |

---

## Shell completion

Shell completion scripts can be generated directly:

```sh
# Bash (e.g. in ~/.bashrc)
source <(guzzle --bash-completion-script "$(command -v guzzle)")

# Zsh (e.g. in ~/.zshrc)
source <(guzzle --zsh-completion-script "$(command -v guzzle)")

# Fish (e.g. in ~/.config/fish/completions/guzzle.fish)
guzzle --fish-completion-script "$(command -v guzzle)" | source
```

