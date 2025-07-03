guzzle 💦
=========

guzzle is a Wayland screen capture CLI tool.

guzzle does not try to do any of these things:
- support X11
- capture anything other than pixels from a screen
- have a GUI

## Usage

```
guzzle [sink] [selection] [capture]
```
`sink` can be one of:
- `copy` - copy the contents to the clipboard (default)
- `save` - save the contents to a file (default if `--file` is present)
- `print` - print the contents to stdout

The output filename for `save` is the current date and time, unless the `--file` argument is given.

The `--file` argument can also be given for other `sink` options, in which case a file will be saved in addition to the specified action.

`selection` can be one of:
- `area` - select a region
- `window` - select a visible window
- `output` - select a visible display output / monitor
- `screen` - all visible outputs
- `anything` - select a region, window, or output (default)

Selections can be saved and reused with the `--area-name` arguments. When an area name is first used, the user will have to make a selection. On subsequent uses, the saved area will be used automatically.

`capture` can be one of:
- `screenshot` - make a screenshot of the selected region (default)
- `video` - record a video of the selected region

Capture can be delayed with the `--delay` argument, which accepts the number of seconds to wait for your make-up crew to finish.

The duration of video recordings can be specified with the `--duration` argument, which accepts the number of seconds for the recording.

### Examples

```
guzzle copy window --file=browser.png --area-name=browser
```
- The user is prompted to select a visible window on the screen.
- The image of the window will be copied to the clipboard and also saved to the file `browser.png`.
- The area currently occupied by the window will be stored and reused on later invocations with `--area-name=browser`.

## Requirements

guzzle looks for the following programs on the `PATH`:
- `slurp` (the original inspiration for this project's name)
- `grim`
- `wl-recorder`
- `wl-copy`

You don't have to worry about any of that if you use Nix.

### Window managers

guzzle talks to window manager APIs to get window and monitor regions.

The currently supported window managers are
- Sway
- Hyprland
- anything else you, the helpful reader, will contribute in PRs :-)
