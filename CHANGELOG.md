# Changelog

## 0.1.0.0

### Changed

- Coverage adapter now uses top-level CAFs (Constant Applicative Forms) to
  record coverage, instead of inline `unsafePerformIO` calls. This ensures
  that each expression location is recorded exactly once, even when compiling
  with `-O0`. Previously, compiling instrumented code without optimisations
  could cause repeated file I/O per expression evaluation, leading to
  timeouts in large test suites. With this change, `-O0` can be used for
  faster compilation without performance regressions at runtime.
