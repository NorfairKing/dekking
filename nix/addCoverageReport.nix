{ haskell
, dekking-report
, addCoverables'
, addCoverage
, compileCoverageReport
}:

args:

# Add a 'report' output to a haskell package

pkg:

((haskell.lib.overrideCabal (addCoverage (addCoverables' args pkg))) (old: {
  postInstall = (old.postInstall or "") + ''
    ${dekking-report}/bin/dekking-report --coverables $coverables --coverage $coverage --output $report
  '';
})).overrideAttrs (old: {
  outputs = (old.outputs or [ ]) ++ [ "report" ];
})
