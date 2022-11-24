{ mkDerivation, aeson, aeson-pretty, autodocodec, base, bytestring
, containers, dekking-plugin, dekking-report, dekking-value, ghc
, lib, mtl, optparse-applicative, path, path-io, pretty-show
, safe-coloured-text, sydtest, sydtest-aeson, sydtest-discover
, text
}:
mkDerivation {
  pname = "dekking-test";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec base bytestring containers
    dekking-plugin dekking-value ghc mtl optparse-applicative path
    path-io pretty-show safe-coloured-text text
  ];
  testHaskellDepends = [
    base containers dekking-report path path-io sydtest sydtest-aeson
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/dekking#readme";
  license = "unknown";
}
