final: prev:
with final.lib;
with final.haskell.lib;
{
  dekking =
    let
      dekking-report = justStaticExecutables final.haskellPackages.dekking-report;
      addDekkingValueDependency = final.callPackage ./addDekkingValueDependency.nix { };
      addCoverables = final.callPackage ./addCoverables.nix { };
      addCoverage = final.callPackage ./addCoverage.nix { inherit addDekkingValueDependency; };
      addCoverablesAndCoverage = pkg: addCoverage (addCoverables pkg);
      addCoverageReport = final.callPackage ./addCoverageReport.nix {
        inherit dekking-report;
        inherit addCoverables addCoverage compileCoverageReport;
      };
      compileCoverageReport = final.callPackage ./compileCoverageReport.nix {
        inherit dekking-report;
      };
    in
    dekking-report.overrideAttrs (old: {
      passthru = (old.passthru or { }) // {
        inherit addDekkingValueDependency addCoverables addCoverage addCoverablesAndCoverage addCoverageReport compileCoverageReport;
        makeCoverageReport = final.callPackage ./makeCoverageReport.nix {
          inherit addDekkingValueDependency addCoverables addCoverage compileCoverageReport;
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
        dekkingPackages // {
          inherit dekkingPackages;
          autodocodec = appendConfigureFlag super.autodocodec "--ghc-option=-w"; # Turn off warnings so the build doesn't fail.
        }
    );
  });
}
