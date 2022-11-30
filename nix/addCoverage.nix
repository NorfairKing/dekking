{ haskell, haskellPackages, addDekkingValueDependency }:

# Add a 'coverage' output to a package that produces a coverage.dat file.

pkg:

(haskell.lib.overrideCabal (addDekkingValueDependency pkg) (old: {
  # We have to use postInstall instead of postCheck in case a package has
  # doCheck turned off.
  # It would then not have a check phase and therefore not run 'postCheck' and
  # fail to produce the 'coverage' output.
  postInstall = (old.postInstall or "") + ''
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
