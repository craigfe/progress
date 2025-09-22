### 0.5.0 (2025-09-19)

- Update to `ocamlformat.0.27.0` (@dinosaure, #49)
- Fix on `Line.eta` (@gasche, #48)
- Remove the `Vector` module and use the `Dynarray` module when it's available

  **breaking change**: `progress` provides a new sub-library `progress.vector`
  which, depending on the OCaml version, corresponds to the `vector` opam
  package for OCaml < 5.2.0 or `Stdlib.Dynarray` for OCaml >= 5.2.0. It unlocks
  the ability to link `progress` with some projects which define their own
  `Vector` module.

### 0.4.0 (2024-20-05)

- Revert the `terminal` API and keep an "happy" path to get size of a tty
  and be compatible with MirageOS (@art-w, @msprotz, #42, #43)
- Use a `float` instead of a `int` in `flow_meter per-second` (@mbarbin, #23, #27)

### 0.3.0 (2024-04-13)

- Be compatible with MirageOS and remove `ocaml_terminal_get_sigwinch` (@art-w, #38)
- Clear all lines in `interject_with` (@Gbury, #30)
- Add `Display.remove_line` (@mbarbin, #26)
- Fix compilation for OCaml 5.2 (reported by @Gbury, fixed by @dinosaure, #40)
- Add `Display.{pause,resume}` (@Gbury, #37)

### 0.2.2 (2023-05-26)

- Fix lower bounds on UTF-8 libraries (@craigfe, 42759d5)
- Fix removed functions in `mtime.2.0.0` (@patricoferris, #31)
- Use actual printer width for elapsed segment (@lsdch, #28)

### 0.2.1 (2021-06-29)

- Fix the count segment of `Progress.counter` (when `pp` is passed) to show the
  running total rather than the latest reported value. (#19; @CraigFe, report
  by @Ngoguey42)
- Fix `Terminal` stubs on MacOS. (#13; @CraigFe, report by @Ngoguey42)
- Fix package tests on Windows. `Progress` does not yet support the Windows and
  Cygwin terminals; this is tracked by #16. (#15; @emillon)

### 0.2.0 (2021-06-26)

Major update of the API, including a number of new features:

- Rename the `Segment` module to `Line`, and improve the set of primitives for
  progress bar construction significantly. This includes time-sensitive segments
  (e.g `bytes_per_sec`, `eta`) and padding segments (`lpad` and `rpad`).
- Add `Progress.interject_with` for interleaving logging with rendering, and
  functions for using `Progress` with `Logs` reporters.
- Add support for adding lines to an ongoing rendering process via `Display`.
- Improve the behaviour of the rendering core: handle terminal width changes /
  respond to user input etc. more cleanly.
- Add many more examples and general improvements to the documentation.
- Extract terminal-specific utilities to a new `Terminal` package.

Also contains a number of smaller fixes:

- Fix the display of minutes and seconds of `Progress.Units.seconds` and
  `Progress_unix.counter`. (#6, @Ngoguey42)
- Raise an exception when attempting to run separate render processes
  simultaneously. (#8, @CraigFe)

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
