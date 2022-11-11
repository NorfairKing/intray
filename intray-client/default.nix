{ mkDerivation, aeson, base, bytestring, containers, intray-api
, intray-data, lib, mergeless, servant, servant-auth-client
, servant-auth-server, servant-client, servant-flatten, text, time
, typed-uuid, validity, validity-bytestring, validity-containers
, validity-text, validity-time, validity-uuid
}:
mkDerivation {
  pname = "intray-client";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers intray-api intray-data mergeless
    servant servant-auth-client servant-auth-server servant-client
    servant-flatten text time typed-uuid validity validity-bytestring
    validity-containers validity-text validity-time validity-uuid
  ];
  description = "Intray Client";
  license = "unknown";
}
