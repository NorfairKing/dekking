{ lib
, linkFarm
, baseHaskellPackages
}:

with lib;

let
  haskellPackages = baseHaskellPackages.extend (self: _: {
    example = self.callPackage ./example { };
    foobar = self.callPackage ./foobar { };
    foobar-gen = self.callPackage ./foobar-gen { };
    syntax = self.callPackage ./syntax { };
  });
  dekking = haskellPackages.dekking;
  examplePkgWithCoverables = dekking.addCoverablesAndCoverage haskellPackages.example;
  foobarPkgWithCoverables = dekking.addCoverablesAndCoverage haskellPackages.foobar;
  foobarGenPkgWithCoverables = dekking.addCoverablesAndCoverage haskellPackages.foobar-gen;
  syntaxPkgWithCoverables = dekking.addCoverablesAndCoverage haskellPackages.syntax;
  example-compiled-report = dekking.compileCoverageReport {
    name = "compiled-coverage-report";
    packages = [ examplePkgWithCoverables ];
  };
  tests = {
    # Single example package
    ## The example package can 'just' build
    example = haskellPackages.example;
    ## We can add coverables to the example package.
    example-with-coverables = examplePkgWithCoverables;
    ## We can compile a report when we add coverables and coverage manually
    inherit example-compiled-report;
    ## We can make a coverage report for the raw package
    example-report = dekking.makeCoverageReport {
      name = "made-coverage-report";
      inherit haskellPackages;
      packages = [ "example" ];
    };
    # We can make a single-package coverage report for the example package
    example-with-report = dekking.addCoverageReport haskellPackages.example;

    passthru-build = example-compiled-report.packages.example;

    # Syntax package, to cover as much haskell syntax as possible
    ## The syntax package can 'just' build
    syntax = haskellPackages.syntax;
    ## We can add coverables to the syntax package and it can still be built
    syntax-with-coverables = syntaxPkgWithCoverables;
    ## We can make a coverage report for the syntax package
    syntax-report = dekking.makeCoverageReport {
      name = "syntax-coverage-report";
      inherit haskellPackages;
      packages = [ "syntax" ];
    };
    ## We can add a coverage report for the syntax package
    syntax-with-report = dekking.addCoverageReport haskellPackages.syntax;



    # Multi-package example
    ## Raw packages
    foobar = haskellPackages.foobar;
    foobar-gen = haskellPackages.foobar-gen;
    ## Packages with coverables
    foobar-with-coverables = foobarPkgWithCoverables;
    foobar-gen-with-coverables = foobarGenPkgWithCoverables;
    ## Coverage report compilation for both raw together
    foobar-report = dekking.makeCoverageReport {
      name = "foobar-made-coverage-report";
      inherit haskellPackages;
      packages = [
        "foobar"
        "foobar-gen"
      ];
    };
    foobar-custom-report = dekking.makeCoverageReport {
      name = "foobar-custom-coverage-report";
      inherit haskellPackages;
      coverables = [
        "foobar"
      ];
      coverage = [
        "foobar-gen"
      ];
    };

    safe-coloured-text-report = dekking.makeCoverageReport {
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

    yesod-report = dekking.makeCoverageReport {
      name = "yesod-coverage-report";
      packages = [
        "yesod"
        "yesod-auth"
        "yesod-auth-oauth"
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
      exceptions = [
        # Once this GHC bug is fixed, we should be able to remove these modules from the exceptions:
        # https://gitlab.haskell.org/ghc/ghc/-/issues/22543
        "Main"
        "Yesod.Auth"
        "Yesod.Auth.OAuth"
        "Yesod.Auth.Dummy"
        "Yesod.Auth.Email"
        "Yesod.Auth.GoogleEmail2"
        "Yesod.Auth.Hardcoded"
        "Yesod.Auth.OpenId"
        "Yesod.Auth.Rpxnow"
        "Yesod.Auth.Util.PasswordStore"
        "Yesod.Core.Content"
        "Yesod.Core.Handler"
        "Yesod.Core.Internal.Response"
        "Yesod.Core.Internal.Run"
        "Yesod.Persist.Core"
        "Yesod.Util.PasswordStore"
        "Yesod.WebSockets"
      ];
    };
  };
in
(linkFarm "e2e-tests" (builtins.attrValues (builtins.mapAttrs
  (name: test: {
    inherit name;
    path = test;
  })
  tests))).overrideAttrs (old: {
  passthru = (old.passthru or { }) // tests;
})
