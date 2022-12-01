{ mkDerivation, aeson, base, http-client, lib, microlens, servant
, servant-client, servant-server, text
}:
mkDerivation {
  pname = "syntax";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base http-client microlens servant servant-client
    servant-server text
  ];
  testHaskellDepends = [ base ];
  license = "unknown";
}
