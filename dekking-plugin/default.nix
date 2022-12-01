{ mkDerivation, aeson, aeson-pretty, autodocodec, base, bytestring
, containers, ghc, ghc-boot, lib, mtl, path, path-io, text
}:
mkDerivation {
  pname = "dekking-plugin";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec base bytestring containers ghc
    ghc-boot mtl path path-io text
  ];
  homepage = "https://github.com/NorfairKing/dekking#readme";
  license = "unknown";
}
