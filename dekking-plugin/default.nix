{ mkDerivation, aeson, aeson-pretty, autodocodec, base, bytestring
, containers, ghc, lib, mtl, path, path-io, text, uniplate
}:
mkDerivation {
  pname = "dekking-plugin";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec base bytestring containers ghc mtl
    path path-io text uniplate
  ];
  homepage = "https://github.com/NorfairKing/dekking#readme";
  license = "unknown";
}
