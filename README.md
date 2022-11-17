# Next-gen test coverage reports for Haskell

## Strategy

1. A GHC plugin is run on every piece of code that requires coverage.
2. This GHC plugin takes a module `FooBar.hs`:
   1. Outputs a list of coverables in `FooBar.hs.coverable`.
      (An old version of this file will be overwritten.)
   2. Does a source-to-source transformation such that each value outputs that
      it's been covered before it is evaluated.
3. When the test is run, a file called `coverage.dat` is accumulated.
   Multiple runs will add to this file.
4. Afterwards, we can create a test coverage report using:
   1. `coverables.dat`
   2. `coverage.dat`
