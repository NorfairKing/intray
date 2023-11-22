{ mkDerivation, aeson, aeson-pretty, autodocodec, autodocodec-yaml
, base, bytestring, containers, envparse, http-client, intray-api
, intray-stripe-client, jose, lib, mergeless, mergeless-persistent
, monad-logger, mtl, optparse-applicative, path, path-io
, persistent, persistent-sqlite, pretty-show, resourcet
, servant-auth-server, servant-server, text, time, typed-uuid, wai
, wai-cors, warp
}:
mkDerivation {
  pname = "intray-server";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec autodocodec-yaml base bytestring
    containers envparse http-client intray-api intray-stripe-client
    jose mergeless mergeless-persistent monad-logger mtl
    optparse-applicative path path-io persistent persistent-sqlite
    pretty-show resourcet servant-auth-server servant-server text time
    typed-uuid wai wai-cors warp
  ];
  executableHaskellDepends = [ base ];
  description = "Intray Server";
  license = "unknown";
  mainProgram = "intray-server";
}
