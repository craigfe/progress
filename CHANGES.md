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
