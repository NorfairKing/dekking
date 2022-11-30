{ pkgs }:

let
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_:_: { })) (
      self: super: {
        example = self.callPackage ./example { };
        foobar = self.callPackage ./foobar { };
        foobar-gen = self.callPackage ./foobar-gen { };
        syntax = self.callPackage ./syntax { };
      }
    );
  });
  examplePkgWithCoverables = pkgs.dekking.addCoverablesAndCoverage haskellPackages.example;
  foobarPkgWithCoverables = pkgs.dekking.addCoverablesAndCoverage haskellPackages.foobar;
  foobarGenPkgWithCoverables = pkgs.dekking.addCoverablesAndCoverage haskellPackages.foobar-gen;
  syntaxPkgWithCoverables = pkgs.dekking.addCoverablesAndCoverage haskellPackages.syntax;
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
    # We can make a single-package coverage report for the example package
    example-with-report = pkgs.dekking.addCoverageReport haskellPackages.example;

    # Syntax package, to cover as much haskell syntax as possible
    ## The syntax package can 'just' build
    syntax = haskellPackages.syntax;
    ## We can add coverables to the syntax package and it can still be built
    syntax-with-coverables = syntaxPkgWithCoverables;
    ## We can make a coverage report for the syntax package
    syntax-report = pkgs.dekking.makeCoverageReport {
      name = "syntax-coverage-report";
      inherit haskellPackages;
      packages = [ "syntax" ];
    };
    ## We can add a coverage report for the syntax package
    syntax-with-report = pkgs.dekking.addCoverageReport haskellPackages.syntax;



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
    foobar-custom-report = pkgs.dekking.makeCoverageReport {
      name = "foobar-custom-coverage-report";
      inherit haskellPackages;
      coverables = [
        "foobar"
      ];
      coverage = [
        "foobar-gen"
      ];
    };

    # External packages' code coverage reports
    fuzzy-time-report = pkgs.dekking.makeCoverageReport {
      name = "fuzzy-time-coverage-report";
      packages = [
        "fuzzy-time"
        "fuzzy-time-gen"
      ];
    };

    safe-coloured-text-report = pkgs.dekking.makeCoverageReport {
      name = "safe-coloured-text-report";
      packages = [
        "safe-coloured-text"
        "safe-coloured-text-layout"
      ];
      coverage = [
        "safe-coloured-text-gen"
        "safe-coloured-text-layout-gen"
      ];
      needToBeLinkedAgainstDekkingValue = [
        "sydtest"
        "genvalidity-sydtest"
      ];
    };

    # Something doesn't work with lenses yet
    # cursor-report = pkgs.dekking.makeCoverageReport {
    #   name = "cursor-coverage-report";
    #   packages = [
    #     "cursor"
    #     "cursor-gen"
    #   ];
    # };
    # It's not clear what is broken here yet.
    # yesod-report = pkgs.dekking.makeCoverageReport {
    #   name = "yesod-coverage-report";
    #   packages = [
    #     "yesod"
    #     # "yesod-auth" # Simplified subsumption
    #     # "yesod-auth-oauth" # Marked as broken
    #     "yesod-bin" # Expression coverage is broken
    #     "yesod-core" # Expression coverage is broken
    #     "yesod-eventsource"
    #     "yesod-form"
    #     "yesod-form-multi"
    #     "yesod-newsfeed"
    #     "yesod-persistent"
    #     "yesod-sitemap"
    #     "yesod-static"
    #     "yesod-test"
    #     "yesod-websockets"
    #   ];
    # };
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
