{ lib, stdenv, haskellPackages, addCoverablesAndCoverage, compileCoverageReport }:
let x = haskellPackages; # Trick we have to use for naming conflicts
in
{ name ? "coverage-report"
, haskellPackages ? x
, packages ? [ ] # List of package names
, extraScript ? ""
}:
let
  newHaskellPackages = haskellPackages.override (old: {
    overrides = lib.composeExtensions (old.overrides or (_: _: { })) (
      self: super:

        # For each package, we override the package to be the source-transformed
        # package instead.
        # Because packages are defined using self.callPackage [...] rather than
        # super.callPackage [...], every reverse dependency of a
        # source-transformed package will now pick up the source-transformed
        # dependency instead of the normal dependency and output coverage
        # correctly.
        builtins.listToAttrs (builtins.map
          (pname: {
            name = pname;
            value = addCoverablesAndCoverage super.${pname};
          })
          packages)
    );
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
}
