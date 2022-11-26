# Next-gen test coverage reports for Haskell

Dekking is a next-generation coverage report tool for Haskell.
It is implemented as a GHC plugin, as opposed to [HPC](https://hackage.haskell.org/package/hpc), which is built into GHC.

Current status: Ready to try out!

## Strategy

There are a few pieces of the puzzle.
The relevant programs are:

* `dekking-plugin`:
   Modifies the parsed source file within GHC as a source-to-source
   transformation plugin.
   At compile-time, this plugin also outputs a `.hs.coverables` file which
   contains information about which parts of the source file are coverable and
   where those pieces are within the source.
   The source is transformed such that, when compiled, the result will output
   coverage information in `coverage.dat`.
* `ghc`: Compiles the resulting modified source code
* `dekking-report`:
   Takes the `*.hs.coverables` files, and any number of `coverage.dat` files,
   and produces a machine-readable `report.json` file, as well as human
   readable HTML files which can be viewed in a browser.

### Source-to-source transformation

The source-to-source transformation works as follows;

#### Top-level bindings

For top-level bindings, for example one like this:

``` haskell
foobar :: IO ()
foobar = pure ()
```

A duplicate is created, with the same type signature, and the following implementation:

``` haskell
foobar :: IO ()
foobar = markAsCovered "foobar" foobarUnlikelyToCollideForCoverageXYZPoopyHead

foobarUnlikelyToCollideForCoverageXYZPoopyHead :: IO ()
foobarUnlikelyToCollideForCoverageXYZPoopyHead = pure ()
```

Here, the `markAsCovered "foobar"` is actually `adaptValue "PackageName Module 2
1 6"`, where `2` is the line number, `1` is the starting column, and `6` is the
ending column of the `foobar` identifier.

We cannot use the same trick as we do for expressions (see below), because
top-level bindings can have multiple patterns and/or guards.

#### Expressions

For expressions, we replace every expression `e` by `markAsCovered "e" e`.
Again, we actually use `adaptValue` with information about where `e` actually
is, but to give an idea of what this looks like, we would transform this
expression:

```
((a + b) * c)
```

into this expression (`f = markExpression`):

```
((f a) + (f b)) * (f c)
```

### The value adapter

The `adaptValue` function mentioned above is implemented in the very small `dekking-value` package, in the `Dekking.ValueLevelAdapter` module.

It looks something like this:

``` haskell
{-# NOINLINE adaptValue #-}
adaptValue :: String -> (forall a. a -> a)
adaptValue logStr = unsafePerformIO $ do
  hPutStrLn coverageHandle logStr
  hFlush coverageHandle
  pure id
```

This function uses the _problem_ of `unsafePerformIO`, namely that the IO is only executed once, as a way to make sure that each expression is only marked as covered once.

### Coverables

Coverables are split up into categories:

* Top-level bindings
* Expressions

Each coverable comes with a location, which is a triple of a line number, a
starting column and an ending column.
This location specifies where the coverable can be found in the source code.

The `*.hs.coverables` files are machine-readable JSON files.

### Coverage

The `coverage.dat` files are text files with a line-by-line description of which pieces of the source have been covered.
Each line is split up into five pieces:

```
<PackageName> <ModuleName> <line> <start> <end>
```
For example:
```
dekking-test-0.0.0.0 Examples.Multi.A 4 1 5
```

### Strategy Overview

![Strategy graph](docs/strategy.svg)

### Nix API

Nix support is a strong requirement of the `dekking` project.
A flake has been provided.
The default package contains the following `passthru` attributes:

* `addCoverables`: Add a `coverables` output to a Haskell package.
* `addCoverage`: Add a `coverage` output to a Haskell package.
* `addCoverablesAndCoverage`: both of the above
* `addCoverageReport`: Add a coverage `report` output to a Haskell package, similar to `doCoverage`.
* `compileCoverageReport`: Compile a coverage report (internal, you probably won't need this.)
* `makeCoverageReport`: Produce a coverage report from multiple Haskell packages.
  Example usage:
  ``` nix
  {
    fuzzy-time-report = dekking.makeCoverageReport {
      name = "fuzzy-time-coverage-report";
      packages = [
        "fuzzy-time"
        "fuzzy-time-gen"
      ];
    };
  }
  ```

See the `e2e-test` directory for many more examples.

### Why a source-to-source transformation?

TODO

## Why not "just" use HPC?

* Strong nix support
* Multi-package coverage reports
* Coupling with GHC

TODO write these out
