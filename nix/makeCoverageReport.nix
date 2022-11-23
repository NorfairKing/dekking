{ lib, stdenv, addCoverablesAndCoverage, compileCoverageReport }:
{ name ? "coverage-report"
, packages ? [ ]
, extraScript ? ""
}:
let packagesWithCoverablesAndCoverage = builtins.map (pkg: addCoverablesAndCoverage pkg) packages;
in
compileCoverageReport {
  inherit name extraScript;
  packages =
    # It's not good enough to just call 'addCoverablesAndCoverage' on each of the packages like this:
    # builtins.map (pkg: addCoverablesAndCoverage pkg) packages;
    # Why? Because every package will have coverables output output, great.
    # However, it still needs to be compiled against the
    # 'addCoverablesAndCoverage' version of its dependencies.
    # So we need to also replace all the dependency of every reverse dependency
    # of every package in the list first.
    builtins.map (pkg: addCoverablesAndCoverage pkg) packages;
}
