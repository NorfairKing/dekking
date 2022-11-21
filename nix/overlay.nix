final: prev:
with final.lib;
with final.haskell.lib;
{
  dekking =
    let
      pkg = final.haskellPackages.dekking;
      addCoverables = final.callPackage ./addCoverables.nix { };
      addCoverage = final.callPackage ./addCoverage.nix { };
      compileCoverageReport = final.callPackage ./compileCoverageReport.nix {
        dekking = pkg;
      };
    in
    (justStaticExecutables pkg).overrideAttrs (old: {
      passthru = (old.passthru or { }) // {
        inherit addCoverables addCoverage compileCoverageReport;
        makeCoverageReport = final.callPackage ./makeCoverageReport.nix {
          inherit addCoverables addCoverage compileCoverageReport;
        };
      };
    });

  haskellPackages = prev.haskellPackages.override
    (old: {
      overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
        self: super: {
          dekking = buildStrictly (self.callPackage ../dekking { });
        }
      );
    });
}
