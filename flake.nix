{
  description = "dekking";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-24.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    fast-myers-diff.url = "github:NorfairKing/fast-myers-diff";
    fast-myers-diff.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , validity
    , safe-coloured-text
    , fast-myers-diff
    , sydtest
    , autodocodec
    }:
    let
      system = "x86_64-linux";
      nixpkgsFor = nixpkgs: import nixpkgs { inherit system; };
      pkgs = nixpkgsFor nixpkgs;
      allOverrides = pkgs.lib.composeManyExtensions [
        (pkgs.callPackage (validity + "/nix/overrides.nix") { })
        (pkgs.callPackage (autodocodec + "/nix/overrides.nix") { })
        (pkgs.callPackage (safe-coloured-text + "/nix/overrides.nix") { })
        (pkgs.callPackage (fast-myers-diff + "/nix/overrides.nix") { })
        (pkgs.callPackage (sydtest + "/nix/overrides.nix") { })
        self.overrides.${system}
      ];
      haskellPackagesFor = nixpkgs: (nixpkgsFor nixpkgs).haskellPackages.extend allOverrides;
      haskellPackages = haskellPackagesFor nixpkgs;
    in
    {
      overrides.${system} = pkgs.callPackage ./nix/overrides.nix { };
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system} = {
        inherit (haskellPackages) dekking; default = haskellPackages.dekking;
      } // haskellPackages.dekkingPackages;
      checks.${system} = {
        release = haskellPackages.dekkingRelease;
        shell = self.devShells.${system}.default;
        e2e-test = import ./e2e-test { inherit (pkgs) lib linkFarm; baseHaskellPackages = haskellPackages; };
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
            tagref.enable = true;
          };
        };
      };
      devShells.${system}.default = haskellPackages.shellFor {
        name = "dekking-shell";
        packages = p: builtins.attrValues p.dekkingPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; [
          cabal-install
          niv
          zlib
        ]) ++ (with pre-commit-hooks.packages.${system};
          [
            cabal2nix
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            tagref
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
    };
}
