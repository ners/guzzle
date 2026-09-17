# Revision history for guzzle

## Unreleased

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
