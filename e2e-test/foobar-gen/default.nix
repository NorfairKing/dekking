{ mkDerivation, base, foobar, genvalidity, genvalidity-sydtest, lib
, sydtest
}:
mkDerivation {
  pname = "foobar-gen";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base foobar genvalidity ];
  testHaskellDepends = [ base foobar genvalidity-sydtest sydtest ];
  license = "unknown";
}
