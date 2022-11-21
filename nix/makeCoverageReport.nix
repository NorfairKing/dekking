{ lib, stdenv, addCoverables, compileCoverageReport }:
{ name ? "coverage-report"
, packages ? [ ]
, extraScript ? ""
}:
compileCoverageReport {
  inherit name extraScript;
  packages = builtins.map addCoverables packages;
}
