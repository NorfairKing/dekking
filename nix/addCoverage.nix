{ haskell }:

# Add a 'coverage' output to a package that produces a coverage.dat file.

pkg:

(haskell.lib.overrideCabal pkg (old: {
  postCheck = (old.postCheck or "") + ''
    cp coverage.dat $coverage
  '';
})).overrideAttrs (old: {
  outputs = (old.outputs or [ ]) ++ [ "coverage" ];
})
