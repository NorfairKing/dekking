{ pkgs }:

let
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_:_: { })) (
      self: super: {
        example = self.callPackage ./example { };
        foobar = self.callPackage ./foobar { };
        foobar-gen = self.callPackage ./foobar-gen { };
      }
    );
  });
  examplePkgWithCoverables = pkgs.dekking.addCoverablesAndCoverage haskellPackages.example;
  foobarPkgWithCoverables = pkgs.dekking.addCoverablesAndCoverage haskellPackages.foobar;
  foobarGenPkgWithCoverables = pkgs.dekking.addCoverablesAndCoverage haskellPackages.foobar-gen;
  tests = {
    # Single example package
    ## The example package can 'just' build
    example = haskellPackages.example;
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
      inherit haskellPackages;
      packages = [ "example" ];
    };

    # Multi-package example
    ## Raw packages
    foobar = haskellPackages.foobar;
    foobar-gen = haskellPackages.foobar-gen;
    ## Packages with coverables
    foobar-with-coverables = foobarPkgWithCoverables;
    foobar-gen-with-coverables = foobarGenPkgWithCoverables;
    ## Coverage report compilation for both raw together
    foobar-report = pkgs.dekking.makeCoverageReport {
      name = "foobar-made-coverage-report";
      inherit haskellPackages;
      packages = [
        "foobar"
        "foobar-gen"
      ];
    };
    # External packages' code coverage reports
    yesod-report = pkgs.dekking.makeCoverageReport {
      name = "yesod-coverage-report";
      packages = [
        "yesod"
        # "yesod-auth" # Simplified subsumption
        # "yesod-auth-oauth" # Marked as broken
        "yesod-bin"
        "yesod-core"
        "yesod-eventsource"
        "yesod-form"
        "yesod-form-multi"
        "yesod-newsfeed"
        "yesod-persistent"
        "yesod-sitemap"
        "yesod-static"
        "yesod-test"
        "yesod-websockets"
      ];
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
