{ lib, haskell, haskellPackages, rsync }:
{ exceptions ? [ ]
}:
pkg:

# Add a 'coverable' output to a package by plugging in the 'Dekking' plugin and
# outputting all the source files and '.coverables' files.

let
  # Inspired by:
  # https://github.com/mpickering/haskell-nix-plugin/blob/2553ab0ff24d0d5752295acb4cf8b1b9dbcb8c76/add-plugin.nix
  pluginOpts = builtins.map (e: "--exception=" + e) exceptions;
  # Build the plugin options.
  stringOpt = arg: "-fplugin-opt=Dekking.Plugin:${arg}";
  stringOpts = lib.concatStringsSep " " (builtins.map stringOpt pluginOpts);
in
(haskell.lib.overrideCabal pkg (old: {
  buildFlags = (old.buildFlags or [ ]) ++ [
    # The '-fplugin' option is required to actually run the plugin at parse-time.
    "--ghc-option=-fplugin=Dekking.Plugin"
    # The '-plugin-package' flag is required for GHC to know in which haskell package to find the plugin with module name ${pluginName}
    # This works because we also add dekking to the 'buildDepends' below.
    "--ghc-option=-plugin-package=dekking-plugin"
    # Turn off warnings because the converted code may produce warnings and we
    # don't care about them.
    # Doing this from inside the plugin doesn't seem to work anymore as of GHC
    # 9.4 so we do it here instead.
    # [ref:TurningOffWarnings]
    "--ghc-option=-w"
    # Here we pass the command-line options to the 'Dekking' plugin
    "--ghc-options=\"${stringOpts}\""
    # The -package option is required because the result of the plugin's
    # source-to-source transformation adds an import of
    # Dekking.ValueLevelAdapter that would not resolve otherwise, without the
    # build-depends in the to-cover haskell package's cabal file.
    "--ghc-option=-package=dekking-value"
  ];
  buildDepends = (old.buildDepends or [ ]) ++ [
    haskellPackages.dekking-plugin
  ];
  # We use libraryHaskellDepends instead of buildDepends because that's what
  # cabal2nix generates, see default.nix in any of the package directories, but
  # I don't think it actually matters.
  libraryHaskellDepends = (old.libraryHaskellDepends or [ ]) ++ [
    haskellPackages.dekking-value
  ];
  # --exclude='*test/': Exclude test directories so we don't produce coverables for the test suite code
  # --include='*/': Include all other directories
  # --include='*.hs': Include Haskell source files
  # --include='*.hs.coverables': Include Haskell source files
  # --include='*.lhs': Include literate Haskell source files
  # --include='*.lhs.coverables': Include literate Haskell source files
  # --exclude='*': Exclude everything else
  postBuild = (old.postBuild or "") + ''
    mkdir -p $coverables
    ${rsync}/bin/rsync -am \
      --exclude='test/' \
      --include='*/' \
      --include='*.hs.coverables' \
      --include='*.lhs.coverables' \
      --exclude='*' \
      . $coverables
  '';
  # Ugly hack because we can't just add flags to the 'test' invocation.
  # Show test output as we go, instead of all at once afterwards.
  testTarget = (old.testTarget or "") + " --show-details=direct";
})).overrideAttrs (old: {
  outputs = (old.outputs or [ ]) ++ [ "coverables" ];
})
