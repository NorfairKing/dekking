{ mkDerivation, base, lib }:
mkDerivation {
  pname = "example";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  license = "unknown";
}
