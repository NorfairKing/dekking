{ pkgs }:

let
  rawExamplePkg = pkgs.haskellPackages.callPackage ./example { };
  tests = {
    # The example package can 'just' build
    sanity = rawExamplePkg;
    # We can add coverables to the exmaple package.
    withCoverables = pkgs.dekking.addCoverables rawExamplePkg;
  };
in
(pkgs.linkFarm "e2e-tests" (builtins.attrValues (builtins.mapAttrs (name: test: { inherit name; path = test; }) tests))).overrideAttrs (old: {
  passthru = (old.passthru or { }) // tests;
})
