final: prev:
with final.lib;
with final.haskell.lib;
{
  dekking =
    let pkg = final.haskellPackages.dekking;
    in (justStaticExecutables pkg).overrideAttrs (old: {
      passthru = (old.passthru or { }) // {
        addCoverables = final.callPackage ./addCoverables.nix { };
        mkCoverageReport = final.callPackage ./mkCoverageReport.nix {
          dekking = pkg;
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
