final: prev:
with final.lib;
with final.haskell.lib;
{
  dekking = (justStaticExecutables final.haskellPackages.dekking).overrideAttrs (old: {
    passthru = (old.passthru or { }) // {
      addCoverables = final.callPackage ./addCoverables.nix { };
      mkCoverageReport = final.callPackage ./mkCoverageReport.nix {
        dekking = final.haskellPackages.dekking;
      };
    };
  });

  haskellPackages = prev.haskellPackages.override
    (old: {
      overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
        self: super: {
          # TODO buildStrictly
          dekking = self.callPackage ../dekking { };
        }
      );
    });
}
