{ mkDerivation, base, lib, validity }:
mkDerivation {
  pname = "foobar";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base validity ];
  license = "unknown";
}
