{ haskell, haskellPackages }:

# Allow linking against dekking-value.

pkg:

haskell.lib.overrideCabal pkg (old: {
  # In order to avoid linker errors, every test suite that depends on a package
  # with the dekking source transformation applied must also be linked against
  # dekking-value.
  # We add the appropriate buildFlags and buildDepends here because it's
  # usually good enough.
  # That is to say we usually definitely have to add these buildFlags and
  # buildDepends here, but some dependencies might be executing test suites
  # that won't automatically be linked against dekking-value.
  # For example, consider the case where we have three packages:
  #
  # A -> B -> C
  #
  # We add coverables to C, and we add coverage to A so that linking A's test
  # suite works.
  # However, if B has a test suite, it will still fail to link.
  buildFlags = (old.buildFlags or [ ]) ++ [
    # build-depends in the to-cover haskell package's cabal file.
    "--ghc-option=-package=dekking-value"
    # Turn off any unused packages warnings that the package might have
    # enabled. In case dekking-value might not be used directly.
    "--ghc-option=-Wno-unused-packages"
  ];
  buildDepends = (old.buildDepends or [ ]) ++ [
    haskellPackages.dekking-value
  ];
})
