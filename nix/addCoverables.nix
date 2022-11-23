{ lib, haskell, haskellPackages, rsync }:
pkg:

# Add a 'coverable' output to a package by plugging in the 'Dekking' plugin and
# outputting all the source files and '.coverables' files.

let
  # Inspired by:
  # https://github.com/mpickering/haskell-nix-plugin/blob/2553ab0ff24d0d5752295acb4cf8b1b9dbcb8c76/add-plugin.nix
  pluginName = "Dekking";
  pluginPackage = haskellPackages.dekking;
  pluginOpts = [ ];
  # Build the plugin options.
  stringOpt = arg: "-fplugin-opt=${pluginName}:${arg}";
  stringOpts = lib.concatStringsSep " " (builtins.map stringOpt pluginOpts);
in
(haskell.lib.overrideCabal pkg (old: {
  buildFlags = (old.buildFlags or [ ]) ++ [
    "--ghc-option=-fno-safe-haskell"
    "--ghc-option=-fno-safe-infer"
    # Turn off all warnings, because the resulting source will cause warnings.
    "--ghc-option=-w"
    # To see what dekking produces, at parse and rename time
    "--ghc-option=-ddump-parsed"
    "--ghc-option=-ddump-rn"
    # The '-fplugin' option is required to actually run the plugin at parse-time.
    "--ghc-option=-fplugin=${pluginName}"
    # The '-plugin-package' flag is required for GHC to know in which haskell package to find the plugin with module name ${pluginName}
    # This works because we also add dekking to the 'buildDepends' below.
    "--ghc-option=-plugin-package=${pluginPackage.pname}"
    # Here we pass the command-line options to the 'Dekking' plugin
    "--ghc-options=\"${stringOpts}\""
    # The -package option is required because the result of the plugin's
    # source-to-source transformation adds an import of
    # Dekking.ValueLevelAdapter that would not resolve otherwise, without the
    # build-depends in the to-cover haskell package's cabal file.
    "--ghc-option=-package${pluginPackage.name}"
  ];
  buildDepends = (old.buildDepends or [ ]) ++ [
    haskellPackages.dekking
  ];
  # --include='*/': Include all directories
  # --include='*.hs': Include Haskell source files
  # --include='*.hs.coverables': Include Haskell source files
  # --include='*.lhs': Include literate Haskell source files
  # --include='*.lhs.coverables': Include literate Haskell source files
  # --exclude='*': Exclude everything else
  postBuild = (old.postBuild or "") + ''
    mkdir -p $coverables
    ${rsync}/bin/rsync -am \
      --include='*/' \
      --include='*.hs' \
      --include='*.hs.coverables' \
      --include='*.lhs' \
      --include='*.lhs.coverables' \
      --exclude='*' \
      . $coverables
  '';
})).overrideAttrs (old: {
  outputs = (old.outputs or [ ]) ++ [ "coverables" ];
})
