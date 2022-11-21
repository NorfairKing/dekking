final: prev:
with final.lib;
with final.haskell.lib;
{
  dekking =
    let
      pkg = final.haskellPackages.dekking;
      addCoverables = final.callPackage ./addCoverables.nix { };
      compileCoverageReport = final.callPackage ./compileCoverageReport.nix {
        dekking = pkg;
      };
    in
    (justStaticExecutables pkg).overrideAttrs (old: {
      passthru = (old.passthru or { }) // {
        inherit addCoverables compileCoverageReport;
        makeCoverageReport = final.callPackage ./makeCoverageReport.nix {
          inherit addCoverables compileCoverageReport;
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
