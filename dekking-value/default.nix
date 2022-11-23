{ mkDerivation, base, filelock, lib }:
mkDerivation {
  pname = "dekking-value";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base filelock ];
  homepage = "https://github.com/NorfairKing/dekking#readme";
  license = "unknown";
}
