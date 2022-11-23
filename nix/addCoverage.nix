{ haskell }:

# Add a 'coverage' output to a package that produces a coverage.dat file.

pkg:

(haskell.lib.overrideCabal pkg (old: {
  postCheck = (old.postCheck or "") + ''
    if [[ -f coverage.dat ]]
    then
      cp coverage.dat $coverage
    else
      touch $coverage
    fi
  '';
})).overrideAttrs (old: {
  outputs = (old.outputs or [ ]) ++ [ "coverage" ];
})
