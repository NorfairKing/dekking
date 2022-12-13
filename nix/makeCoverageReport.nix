{ lib
, haskell
, haskellPackages
, addDekkingValueDependency
, addCoverables'
, addCoverage
, compileCoverageReport
}:
let x = haskellPackages; # Trick we have to use for naming conflicts
in
{ name ? "coverage-report"
, haskellPackages ? x
  # List of package names
  # These packages will be reported and their test suite's coverage collected.
, packages ? [ ]
  # List of package names
  # These packages will be reported but their test suite's coverage NOT collected.
, coverables ? [ ]
  # List of package names
  # These packages will NOT be reported but thuir test suite's coverage will be collected.
, coverage ? [ ]
  # List of package names
  # These packages will be linked against dekking-value to prevent linking errors.
  # See ./nix/addDekkingValueDependency.nix for more details.
, needToBeLinkedAgainstDekkingValue ? [ ] # List of package names
  # Modules that will not be source-transformed
, exceptions ? [ ]
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
  addDekkingValueDependencyOverride = _: super:
    builtins.listToAttrs
      (builtins.map
        (pname: {
          name = pname;
          value = addDekkingValueDependency super.${pname};
        })
        needToBeLinkedAgainstDekkingValue);
  addCoverableOverride = _: super:
    builtins.listToAttrs (builtins.map
      (pname: {
        name = pname;
        value = addCoverables' { inherit exceptions; } super.${pname};
      })
      allCoverables);
  addCoverageOverride = _: super:
    builtins.listToAttrs (builtins.map
      (pname: {
        name = pname;
        value = addCoverage super.${pname};
      })
      allCoverage);
  # We turn on 'doCheck' for all coverage packages just so it can be off by
  # default and still be used in a coverage report.
  addDoCheckOverride = _: super:
    builtins.listToAttrs (builtins.map
      (pname: {
        name = pname;
        value = haskell.lib.doCheck super.${pname};
      })
      allCoverage);
  newHaskellPackages = haskellPackages.override (old: {
    overrides = lib.composeExtensions (old.overrides or (_: _: { }))
      (lib.composeExtensions
        (lib.composeExtensions addDekkingValueDependencyOverride addDoCheckOverride)
        (lib.composeExtensions addCoverableOverride addCoverageOverride));
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
