{ mkDerivation, aeson, base, intray-api, lib, mergeless, servant
, servant-auth-client, servant-client, servant-flatten, text
, typed-uuid
}:
mkDerivation {
  pname = "intray-client";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base intray-api mergeless servant servant-auth-client
    servant-client servant-flatten text typed-uuid
  ];
  description = "Intray Client";
  license = "unknown";
}
