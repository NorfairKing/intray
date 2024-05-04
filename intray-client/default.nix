{ mkDerivation, aeson, base, containers, intray-api, lib, mergeless
, servant, servant-auth-client, servant-client, servant-flatten
, text, time, typed-uuid
}:
mkDerivation {
  pname = "intray-client";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers intray-api mergeless servant
    servant-auth-client servant-client servant-flatten text time
    typed-uuid
  ];
  description = "Intray Client";
  license = "unknown";
}
