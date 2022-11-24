{ mkDerivation, base, lib }:
mkDerivation {
  pname = "dekking-value";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/dekking#readme";
  license = "unknown";
}
