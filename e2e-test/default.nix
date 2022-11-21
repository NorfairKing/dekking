{ pkgs }:

let
  rawExamplePkg = pkgs.haskellPackages.callPackage ./example { };
  examplePkgWithCoverables = pkgs.dekking.addCoverables rawExamplePkg;
  tests = {
    # The example package can 'just' build
    sanity = rawExamplePkg;
    # We can add coverables to the example package.
    withCoverables = examplePkgWithCoverables;
    report = pkgs.dekking.mkCoverageReport {
      name = "e2e-coverage-report";
      packages = [ examplePkgWithCoverables ];
    };
  };
in
(pkgs.linkFarm "e2e-tests" (builtins.attrValues (builtins.mapAttrs (name: test: { inherit name; path = test; }) tests))).overrideAttrs (old: {
  passthru = (old.passthru or { }) // tests;
})
