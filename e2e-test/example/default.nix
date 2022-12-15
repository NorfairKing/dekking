{ mkDerivation, base, dekking-plugin, dekking-value, lib }:
mkDerivation {
  pname = "example";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base dekking-plugin dekking-value ];
  testHaskellDepends = [ base ];
  license = "unknown";
}
