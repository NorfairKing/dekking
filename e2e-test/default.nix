{ pkgs }:

let
  rawExamplePkg = pkgs.haskellPackages.callPackage ./example { };
  rawFoobarPkg = pkgs.haskellPackages.callPackage ./foobar { };
  rawFoobarGenPkg = pkgs.haskellPackages.callPackage ./foobar-gen { foobar = rawFoobarPkg; };
  examplePkgWithCoverables = pkgs.dekking.addCoverablesAndCoverage rawExamplePkg;
  foobarPkgWithCoverables = pkgs.dekking.addCoverablesAndCoverage rawFoobarPkg;
  foobarGenPkgWithCoverables = pkgs.dekking.addCoverablesAndCoverage rawFoobarGenPkg;
  tests = {
    # Single example package
    ## The example package can 'just' build
    example = rawExamplePkg;
    ## We can add coverables to the example package.
    example-with-coverables = examplePkgWithCoverables;
    ## We can compile a report when we add coverables and coverage manually
    example-compiled-report = pkgs.dekking.compileCoverageReport {
      name = "compiled-coverage-report";
      packages = [ examplePkgWithCoverables ];
    };
    ## We can make a coverage report for the raw package
    example-report = pkgs.dekking.makeCoverageReport {
      name = "made-coverage-report";
      packages = [ rawExamplePkg ];
    };
    # Multi-package example
    ## Raw packages
    foobar = rawFoobarPkg;
    foobar-gen = rawFoobarGenPkg;
    ## Packages with coverables
    foobar-with-coverables = foobarPkgWithCoverables;
    foobar-gen-with-coverables = foobarGenPkgWithCoverables;
    ## Coverage report compilation for both together
    foobar-compiled-report = pkgs.dekking.compileCoverageReport {
      name = "foobar-compiled-coverage-report";
      packages = [ foobarPkgWithCoverables foobarGenPkgWithCoverables ];
    };
    ## Coverage report compilation for both raw together
    foobar-report = pkgs.dekking.makeCoverageReport {
      name = "foobar-made-coverage-report";
      packages = [ rawFoobarPkg rawFoobarGenPkg ];
    };

    ## E2E tests of coverage for external packages.
    safe-coloured-text = pkgs.dekking.makeCoverageReport {
      name = "safe-coloured-text-report";
      packages = (builtins.attrValues pkgs.haskellPackages.safeColouredTextPackages);
    };
  };
in
(pkgs.linkFarm "e2e-tests" (builtins.attrValues (builtins.mapAttrs
  (name: test: {
    inherit name;
    path = test;
  })
  tests))).overrideAttrs (old: {
  passthru = (old.passthru or { }) // tests;
})
