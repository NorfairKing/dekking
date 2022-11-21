{ stdenv, dekking }:
{ name ? "coverage"
, packages ? [ ]
, extraScript ? ""
}:
# foobar: Library
# foobar-gen: Library + test suite

# let
#   packagesWithCoverables = ;
# in
stdenv.mkDerivation {
  inherit name;
  srcs = [ ];
  buildInputs = [ dekking ];
  buildCommand = ''
    ${extraScript}

    # Make coverage report
    dekking > $out
  '';
}
