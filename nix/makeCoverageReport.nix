{ lib, stdenv, addCoverables, addCoverage, compileCoverageReport }:
{ name ? "coverage-report"
, packages ? [ ]
, extraScript ? ""
}:
compileCoverageReport {
  inherit name extraScript;
  packages = builtins.map (pkg: addCoverage (addCoverables pkg)) packages;
}
