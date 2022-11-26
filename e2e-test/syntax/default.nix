{ mkDerivation, base, lib, text }:
mkDerivation {
  pname = "syntax";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base text ];
  testHaskellDepends = [ base ];
  license = "unknown";
}
