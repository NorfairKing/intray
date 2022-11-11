{ mkDerivation, aeson, autodocodec, base, base64-bytestring
, blaze-html, blaze-markup, bytestring, containers, cookie
, hashable, intray-cli-data, intray-data, JuicyPixels
, JuicyPixels-extra, lib, mergeless, monad-logger, servant
, servant-auth, servant-auth-server, servant-blaze, text, time
, typed-uuid, uuid, validity, validity-bytestring
, validity-containers, validity-text, validity-time, validity-uuid
, wai
}:
mkDerivation {
  pname = "intray-api";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base base64-bytestring blaze-html blaze-markup
    bytestring containers cookie hashable intray-cli-data intray-data
    JuicyPixels JuicyPixels-extra mergeless monad-logger servant
    servant-auth servant-auth-server servant-blaze text time typed-uuid
    uuid validity validity-bytestring validity-containers validity-text
    validity-time validity-uuid wai
  ];
  description = "Intray API";
  license = "unknown";
}
