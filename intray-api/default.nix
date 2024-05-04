{ mkDerivation, aeson, autodocodec, base, base16-bytestring
, base64-bytestring, bcrypt, bytestring, containers, http-api-data
, JuicyPixels, JuicyPixels-extra, lib, mergeless, monad-logger
, path-pieces, persistent, random, servant, servant-auth
, servant-auth-server, text, time, typed-uuid, uuid, validity
, validity-bytestring, validity-text, validity-time, wai
}:
mkDerivation {
  pname = "intray-api";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base base16-bytestring base64-bytestring bcrypt
    bytestring containers http-api-data JuicyPixels JuicyPixels-extra
    mergeless monad-logger path-pieces persistent random servant
    servant-auth servant-auth-server text time typed-uuid uuid validity
    validity-bytestring validity-text validity-time wai
  ];
  description = "Intray API";
  license = "unknown";
}
