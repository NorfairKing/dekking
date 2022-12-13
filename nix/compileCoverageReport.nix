{ lib, stdenv, dekking-report }:
{ name ? "coverage-report"
, packages ? [ ]
, coverables ? [ ]
, coverage ? [ ]
}:
let
  allCoverables = coverables ++ packages;
  allCoverage = coverage ++ packages;
  coverablesOption = package: "--coverables ${package.coverables}";
  coverablesOptions = lib.concatStringsSep " " (builtins.map coverablesOption allCoverables);
  coverageOption = package: "--coverage ${package.coverage}";
  coverageOptions = lib.concatStringsSep " " (builtins.map coverageOption allCoverage);
in
stdenv.mkDerivation {
  inherit name;
  srcs = [ ];
  buildInputs = [ dekking-report ];
  buildCommand = ''
    # Make coverage report
    set -x
    dekking-report ${coverablesOptions} ${coverageOptions} --output $out
    set +x
  '';
}
