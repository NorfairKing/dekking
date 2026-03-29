{ lib
, stdenv
, haskell
, jq
, rsync
, symlinkJoin
, ...
}:
with lib;
with haskell.lib;
self: super:
let
  dekkingPackages = {
    dekking-plugin = buildStrictly (self.callPackage ../dekking-plugin { });
    dekking-report = buildStrictly (self.callPackage ../dekking-report { });
    dekking-value = buildStrictly (self.callPackage ../dekking-value { });
  };
  dekking =
    let
      dekking-report = justStaticExecutables self.dekking-report;
      addCoverables' = import ./addCoverables.nix {
        inherit lib haskell rsync; haskellPackages = self;
      };
      addCoverables = self.dekking.addCoverables' { };
      addCoverage = import ./addCoverage.nix { inherit haskell; };
      addCoverablesAndCoverage = pkg: addCoverage (addCoverables pkg);
      addCoverageReport' = import ./addCoverageReport.nix {
        inherit haskell;
        inherit dekking-report;
        inherit addCoverables' addCoverage;
      };
      addCoverageReport = self.dekking.addCoverageReport' { };
      compileCoverageReport = import ./compileCoverageReport.nix {
        inherit lib stdenv dekking-report;
      };
      assertCoverageThreshold = import ./assertCoverageThreshold.nix {
        inherit lib stdenv jq;
      };
      requireCoverage = import ./requireCoverage.nix {
        inherit lib stdenv jq;
      };
    in
    dekking-report.overrideAttrs (old: {
      passthru = (old.passthru or{ }) // {
        inherit
          addCoverables
          addCoverables'
          addCoverage
          addCoverablesAndCoverage
          addCoverageReport
          addCoverageReport'
          assertCoverageThreshold
          compileCoverageReport
          requireCoverage;
        makeCoverageReport = import ./makeCoverageReport.nix {
          inherit lib stdenv haskell addCoverables' addCoverage assertCoverageThreshold compileCoverageReport requireCoverage;
          haskellPackages = self;
        };
      };
    });
in
dekkingPackages // {
  inherit dekking;
  inherit dekkingPackages;
  dekkingRelease = symlinkJoin {
    name = "dekking-release";
    paths = attrValues self.dekkingPackages;
  };
}
