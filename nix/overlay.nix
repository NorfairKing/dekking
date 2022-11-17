final: prev:
with final.lib;
with final.haskell.lib;
{
  dekking = justStaticExecutables final.haskellPackages.dekking;

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
      self: super:
        {
          dekking =
            buildFromSdist (overrideCabal (self.callPackage (../dekking) { }) (old: {
              configureFlags = (old.configureFlags or [ ]) ++ [
                # Optimisations
                "--ghc-options=-O2"
                # Extra warnings
                "--ghc-options=-Wall"
                "--ghc-options=-Wincomplete-uni-patterns"
                "--ghc-options=-Wincomplete-record-updates"
                "--ghc-options=-Wpartial-fields"
                "--ghc-options=-Widentities"
                "--ghc-options=-Wredundant-constraints"
                "--ghc-options=-Wcpp-undef"
                "--ghc-options=-Werror"
              ];
              doBenchmark = true;
              doHaddock = false;
              doCoverage = false;
              doHoogle = false;
              doCheck = false; # Only check the release version.
              hyperlinkSource = false;
              enableLibraryProfiling = false;
              enableExecutableProfiling = false;
              # Ugly hack because we can't just add flags to the 'test' invocation.
              # Show test output as we go, instead of all at once afterwards.
              testTarget = (old.testTarget or "") + " --show-details=direct";
            }));
        }
    );
  });
}
