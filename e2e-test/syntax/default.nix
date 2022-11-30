{ mkDerivation, aeson, base, http-client, lib, servant
, servant-client, servant-server, text
}:
mkDerivation {
  pname = "syntax";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base http-client servant servant-client servant-server text
  ];
  testHaskellDepends = [ base ];
  license = "unknown";
}
