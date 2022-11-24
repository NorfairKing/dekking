{ mkDerivation, aeson, aeson-pretty, autodocodec, base, bytestring
, containers, ghc, lib, mtl, path, path-io
}:
mkDerivation {
  pname = "dekking-plugin";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec base bytestring containers ghc mtl
    path path-io
  ];
  homepage = "https://github.com/NorfairKing/dekking#readme";
  license = "unknown";
}
