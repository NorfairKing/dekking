{ lib, stdenv, jq }:
{ name ? "coverage-threshold-check"
, report # The coverage report derivation (has uncovered.json)
, threshold # Minimum coverage percentage (0-100)
}:

stdenv.mkDerivation {
  inherit name;
  srcs = [ ];
  nativeBuildInputs = [ jq ];
  buildCommand = ''
    covered=$(jq '[.packages[].modules[].covered] | add // 0' ${report}/uncovered.json)
    total=$(jq '[.packages[].modules[].total] | add // 0' ${report}/uncovered.json)

    if [ "$total" -eq 0 ]; then
      percentage=0
    else
      percentage=$(( covered * 100 / total ))
    fi

    echo "Coverage: $covered / $total ($percentage%)"
    echo "Threshold: ${toString threshold}%"

    if [ "$percentage" -lt "${toString threshold}" ]; then
      echo "FAIL: Coverage $percentage% is below the threshold of ${toString threshold}%"
      exit 1
    fi

    echo "PASS: Coverage $percentage% meets the threshold of ${toString threshold}%"
    mkdir -p $out
    echo "$percentage" > $out/percentage.txt
  '';
}
