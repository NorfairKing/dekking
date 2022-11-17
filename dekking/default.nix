{ mkDerivation, aeson, aeson-pretty, autodocodec, base, bytestring
, ghc, lib, mtl, path, path-io, pretty-show
}:
mkDerivation {
  pname = "dekking";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec base bytestring ghc mtl path path-io
    pretty-show
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/dekking#readme";
  license = "unknown";
  mainProgram = "dekking";
}
