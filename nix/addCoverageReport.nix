{ haskell
, dekking-report
, addCoverables
, addCoverage
, compileCoverageReport
}:

# Add a 'report' output to a haskell package

pkg:

((haskell.lib.overrideCabal (addCoverage (addCoverables pkg))) (old: {
  postInstall = (old.postInstall or "") + ''
    ${dekking-report}/bin/dekking-report --coverables $coverables --coverage $coverage --output $report
  '';
})).overrideAttrs (old: {
  outputs = (old.outputs or [ ]) ++ [ "report" ];
})
