final: prev:
with final.lib;
with final.haskell.lib;
{
  dekking =
    let
      dekking-report = justStaticExecutables final.haskellPackages.dekking-report;
      addCoverables = final.callPackage ./addCoverables.nix { };
      addCoverage = final.callPackage ./addCoverage.nix { };
      addCoverablesAndCoverage = pkg: addCoverage (addCoverables pkg);
      compileCoverageReport = final.callPackage ./compileCoverageReport.nix {
        inherit dekking-report;
      };
    in
    dekking-report.overrideAttrs (old: {
      passthru = (old.passthru or { }) // {
        inherit addCoverables addCoverage addCoverablesAndCoverage compileCoverageReport;
        makeCoverageReport = final.callPackage ./makeCoverageReport.nix {
          inherit addCoverablesAndCoverage compileCoverageReport;
        };
      };
    });

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
      self: super:
        let
          dekkingPackages = {
            dekking-plugin = buildStrictly (self.callPackage ../dekking-plugin { });
            dekking-report = buildStrictly (self.callPackage ../dekking-report { });
            dekking-value = buildStrictly (self.callPackage ../dekking-value { });
          };
        in
        dekkingPackages // { inherit dekkingPackages; }
    );
  });
}
