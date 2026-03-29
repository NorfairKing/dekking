{ lib, stdenv, jq }:
{ name ? "require-coverage-check"
, report # The coverage report derivation (has uncovered.json)
}:

stdenv.mkDerivation {
  inherit name;
  srcs = [ ];
  nativeBuildInputs = [ jq ];
  buildCommand = ''
    covered=$(jq '[.packages[].modules[].covered] | add // 0' ${report}/uncovered.json)

    if [ "$covered" -eq 0 ]; then
      echo "FAIL: No covered expressions found."
      echo "Unless you have no tests at all, this means your tests were not run."
      echo "You can turn this check off with mustCover = false;"
      exit 1
    fi

    echo "PASS: Found $covered covered expressions."
    mkdir -p $out
    echo "$covered" > $out/covered.txt
  '';
}
