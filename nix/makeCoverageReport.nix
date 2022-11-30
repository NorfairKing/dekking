{ lib, stdenv, haskellPackages, addCoverables, addCoverage, compileCoverageReport }:
let x = haskellPackages; # Trick we have to use for naming conflicts
in
{ name ? "coverage-report"
, haskellPackages ? x
, packages ? [ ] # List of package names
, coverables ? [ ] # List of package names
, coverage ? [ ] # List of package names
, extraScript ? ""
}:
let
  allCoverables = coverables ++ packages;
  allCoverage = coverage ++ packages;
  # For each package, we override the package to be the source-transformed
  # package instead.
  # Because packages are defined using self.callPackage [...] rather than
  # super.callPackage [...], every reverse dependency of a
  # source-transformed package will now pick up the source-transformed
  # dependency instead of the normal dependency and output coverage
  # correctly.
  addCoverableOverride = self: super:
    builtins.listToAttrs (builtins.map
      (pname: {
        name = pname;
        value = addCoverables super.${pname};
      })
      allCoverables);
  addCoverageOverride = self: super:
    builtins.listToAttrs (builtins.map
      (pname: {
        name = pname;
        value = addCoverage super.${pname};
      })
      allCoverage);
  newHaskellPackages = haskellPackages.override (old: {
    overrides = lib.composeExtensions (old.overrides or (_: _: { }))
      (lib.composeExtensions addCoverableOverride addCoverageOverride);
  });
in
compileCoverageReport {
  inherit name extraScript;
  # It's not good enough to just call 'addCoverablesAndCoverage' on each of the packages like this:
  # packages = builtins.map (pkg: addCoverablesAndCoverage pkg) packages;
  # Why? Because every package will have coverables output output, great.
  # However, it still needs to be compiled against the
  # 'addCoverablesAndCoverage' version of its dependencies.
  # So we need to also replace all the dependency of every reverse dependency
  # of every package in the list first.
  # builtins.map (pkg: newHaskellPackages.${pkg.pname}) packages;
  packages = builtins.map (pname: newHaskellPackages.${pname}) packages;
  coverage = builtins.map (pname: newHaskellPackages.${pname}) coverage;
  coverables = builtins.map (pname: newHaskellPackages.${pname}) coverables;
}
