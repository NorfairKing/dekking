{ pkgs }:

let
  rawExamplePkg = pkgs.haskellPackages.callPackage ./example { };
  examplePkgWithCoverables = pkgs.dekking.addCoverables rawExamplePkg;
  tests = {
    # The example package can 'just' build
    sanity = rawExamplePkg;
    # We can add coverables to the example package.
    withCoverables = examplePkgWithCoverables;
    compiled-report = pkgs.dekking.compileCoverageReport {
      name = "compiled-coverage-report";
      packages = [ examplePkgWithCoverables ];
    };
    report = pkgs.dekking.makeCoverageReport {
      name = "made-coverage-report";
      packages = [ rawExamplePkg ];
    };
  };
in
(pkgs.linkFarm "e2e-tests" (builtins.attrValues (builtins.mapAttrs (name: test: { inherit name; path = test; }) tests))).overrideAttrs (old: {
  passthru = (old.passthru or { }) // tests;
})
