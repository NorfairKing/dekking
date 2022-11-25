{ mkDerivation, aeson, autodocodec, base, blaze-html, bytestring
, containers, dekking-plugin, lib, optparse-applicative, path
, path-io, pretty-show, shakespeare
}:
mkDerivation {
  pname = "dekking-report";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec base blaze-html bytestring containers
    dekking-plugin optparse-applicative path path-io pretty-show
    shakespeare
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/dekking#readme";
  license = "unknown";
  mainProgram = "dekking-report";
}
