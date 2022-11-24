{ lib, stdenv, dekking-report }:
{ name ? "coverage-report"
, packages ? [ ]
, extraScript ? ""
}:
let
  coverablesOption = package: "--coverables ${package.coverables}";
  coverablesOptions = lib.concatStringsSep " " (builtins.map coverablesOption packages);
  coverageOption = package: "--coverage ${package.coverage}";
  coverageOptions = lib.concatStringsSep " " (builtins.map coverageOption packages);
in
stdenv.mkDerivation {
  inherit name;
  srcs = [ ];
  buildInputs = [ dekking-report ];
  buildCommand = ''

    ${extraScript}

    # Make coverage report
    set -x
    dekking-report ${coverablesOptions} ${coverageOptions} > $out
    set +x
  '';
}
