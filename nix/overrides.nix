{ lib
, stdenv
, haskell
, rsync
, symlinkJoin
, ...
}:
with lib;
with haskell.lib;
self: _:
let
  dekkingPackages = {
    dekking-plugin = buildStrictly (self.callPackage ../dekking-plugin { });
    dekking-report = buildStrictly (self.callPackage ../dekking-report { });
    dekking-value = buildStrictly (self.callPackage ../dekking-value { });
  };
  dekking =
    let
      dekking-report = justStaticExecutables self.dekking-report;
      addDekkingValueDependency = import ./addDekkingValueDependency.nix {
        inherit haskell; haskellPackages = self;
      };
      addCoverables' = import ./addCoverables.nix {
        inherit lib haskell rsync; haskellPackages = self;
      };
      addCoverables = self.dekking.addCoverables' { };
      addCoverage = import ./addCoverage.nix { inherit haskell addDekkingValueDependency; };
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
    in
    dekking-report.overrideAttrs (old: {
      passthru = (old.passthru or{ }) // {
        inherit
          addDekkingValueDependency
          addCoverables
          addCoverables'
          addCoverage
          addCoverablesAndCoverage
          addCoverageReport
          addCoverageReport'
          compileCoverageReport;
        makeCoverageReport = import ./makeCoverageReport.nix {
          inherit lib haskell addDekkingValueDependency addCoverables' addCoverage compileCoverageReport;
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
