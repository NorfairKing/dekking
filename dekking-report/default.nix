{ mkDerivation, aeson, aeson-pretty, autodocodec, base, bytestring
, containers, dekking-plugin, dekking-value, ghc, lib, mtl
, optparse-applicative, path, path-io, pretty-show
, safe-coloured-text, sydtest, sydtest-aeson, sydtest-discover
, text
}:
mkDerivation {
  pname = "dekking-report";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec base bytestring containers
    dekking-plugin ghc mtl optparse-applicative path path-io
    pretty-show safe-coloured-text text
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base containers dekking-value path path-io sydtest sydtest-aeson
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/dekking#readme";
  license = "unknown";
  mainProgram = "dekking-report";
}
