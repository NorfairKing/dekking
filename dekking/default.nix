{ mkDerivation, aeson, aeson-pretty, autodocodec, base, bytestring
, containers, dekking-value, filelock, ghc, lib, mtl
, optparse-applicative, path, path-io, pretty-show
, safe-coloured-text, sydtest, sydtest-aeson, sydtest-discover
, text
}:
mkDerivation {
  pname = "dekking";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec base bytestring containers
    dekking-value filelock ghc mtl optparse-applicative path path-io
    pretty-show safe-coloured-text text
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base containers dekking-value path path-io sydtest sydtest-aeson
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/dekking#readme";
  license = "unknown";
  mainProgram = "dekking";
}
