{ mkDerivation, aeson, autodocodec, base, base16-bytestring, bcrypt
, bytestring, containers, hashable, http-api-data, lib, path-pieces
, persistent, persistent-template, random, text, time, typed-uuid
, uuid, validity, validity-bytestring, validity-containers
, validity-text, validity-time, validity-uuid
}:
mkDerivation {
  pname = "intray-data";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base base16-bytestring bcrypt bytestring
    containers hashable http-api-data path-pieces persistent
    persistent-template random text time typed-uuid uuid validity
    validity-bytestring validity-containers validity-text validity-time
    validity-uuid
  ];
  description = "Intray Data";
  license = "unknown";
}
