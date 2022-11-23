final: prev:
with final.lib;
with final.haskell.lib;
{
  dekking =
    let
      pkg = final.haskellPackages.dekking;
      addCoverables = final.callPackage ./addCoverables.nix { };
      addCoverage = final.callPackage ./addCoverage.nix { };
      addCoverablesAndCoverage = pkg: addCoverage (addCoverables pkg);
      compileCoverageReport = final.callPackage ./compileCoverageReport.nix {
        dekking = pkg;
      };
    in
    (justStaticExecutables pkg).overrideAttrs (old: {
      passthru = (old.passthru or { }) // {
        inherit addCoverables addCoverage addCoverablesAndCoverage compileCoverageReport;
        makeCoverageReport = final.callPackage ./makeCoverageReport.nix {
          inherit addCoverablesAndCoverage compileCoverageReport;
        };
      };
    });

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
      self: super: {
        dekking-value = buildStrictly (self.callPackage ../dekking-value { });
        dekking = buildStrictly (self.callPackage ../dekking { });
      }
    );
  });
}
