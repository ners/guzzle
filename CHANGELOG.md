# Revision history for guzzle

## 0.3.0.0 -- 2026-10-10

* Add `--all` to select every candidate of the chosen kind.
* Add plural aliases for selection kinds.
* Add `--no-history` to disable storing and recalling areas.
* Add `--no-notify` to disable desktop notifications. Breaking: remove `GUZZLE_NOTIFY`.
* Make `--file` a template with placeholders.
* Add `--format` to `select`.
* Allow several selection kinds at once.
* Add `--area-format`, `--window-format`, `--output-format`, and `--screen-format` to `select`.
* Make interactive picks exact.
* Add `%I` placeholder for the window identifier.
* Apply `--scale` to videos. Reject non-positive values.
* Support several items in `copy`, `save`, and `print`.
* Capture several items concurrently.
* Add `Region.Kind`.
* Add `--quiet` and `--debug`. Breaking: remove `GUZZLE_DEBUG`.
* Support outputs and screen on Hyprland.

## 0.2.0.0 -- 2026-09-17

* Add niri support.
* Add `select` command.
* Fix `print` sink.
* Add `--last-area`, `--cursor`, `--audio`, `--audio-device`, `--format`, `--quality`, `--scale`, `--framerate`.
* Offer the last selected area as a candidate when selecting `anything` or `area`.
* Infer content type from file extension, if given.
* Pass content type to `wl-copy` when copying.
* Only printed executed commands if the `GUZZLE_DEBUG` environment variable is set.
* Send a desktop notification via `notify-send` when a capture is saved or copied. Use `GUZZLE_NOTIFY=0` to disable desktop notifications.

## 0.1.0.0 -- 2025-07-04

* First version. Released on an unsuspecting world.
