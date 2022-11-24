{ mkDerivation, aeson, aeson-pretty, autodocodec, base, bytestring
, containers, dekking-value, filelock, ghc, lib, mtl
, optparse-applicative, path, path-io, pretty-show
, safe-coloured-text, text
}:
mkDerivation {
  pname = "dekking-plugin";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec base bytestring containers
    dekking-value filelock ghc mtl optparse-applicative path path-io
    pretty-show safe-coloured-text text
  ];
  homepage = "https://github.com/NorfairKing/dekking#readme";
  license = "unknown";
}
