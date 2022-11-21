{ mkDerivation, aeson, aeson-pretty, autodocodec, base, bytestring
, containers, ghc, lib, mtl, optparse-applicative, path, path-io
, pretty-show, sydtest, sydtest-aeson, sydtest-discover
}:
mkDerivation {
  pname = "dekking";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec base bytestring containers ghc mtl
    optparse-applicative path path-io pretty-show
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base path path-io sydtest sydtest-aeson ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/dekking#readme";
  license = "unknown";
  mainProgram = "dekking";
}
