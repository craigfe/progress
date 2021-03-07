### Unreleased

- Fix the display of minutes and seconds of `Progress.Units.seconds` and
  `Progress_unix.counter`. (#6, @Ngoguey42)

- Hide the terminal cursor during rendering. (#8, @CraigFe)

- Raise an exception when attempting to run separate render processes
  simultaneously. (#8, @CraigFe)

- Add `Progress.interject_with` for interleaving logging with rendering, and
  the `progress.logs` for pre-packaged `Progress`-aware `Logs` reporters.
  (#9, @CraigFe)

### 0.1.1 (2020-10-13)

- Rename `Progress.with_display` to `Progress.with_reporters`. (#3, @CraigFe)

- Change the default display mode of progress bars to `ASCII` rather than
  `UTF8`. (#2, @CraigFe)

- Change `Segment.box_dynamic` to take a function rather than a reference. (#1,
  @CraigFe)

- Fix a bug causing multi-line layouts to occasionally not adapt to terminal
  size changes. (#1, @CraigFe)

### 0.1.0 (2020-10-12)

Initial release.
