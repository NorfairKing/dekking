{ mkDerivation, aeson, aeson-pretty, autodocodec, base, bytestring
, ghc, lib, mtl, optparse-applicative, path, path-io, pretty-show
, sydtest, sydtest-discover
}:
mkDerivation {
  pname = "dekking";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec base bytestring ghc mtl
    optparse-applicative path path-io pretty-show
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base sydtest ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/dekking#readme";
  license = "unknown";
  mainProgram = "dekking";
}
