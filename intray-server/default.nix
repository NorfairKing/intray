{ mkDerivation, aeson, aeson-pretty, autodocodec, autodocodec-yaml
, base, bytestring, conduit, containers, envparse, exceptions
, file-embed, http-client, intray-api, intray-data
, intray-stripe-client, jose, lib, markdown, mergeless
, mergeless-persistent, monad-logger, mtl, optparse-applicative
, path, path-io, persistent, persistent-sqlite, pretty-show
, resourcet, safe, servant, servant-auth-server, servant-docs
, servant-server, text, time, typed-uuid, unordered-containers, wai
, wai-cors, warp, yaml
}:
mkDerivation {
  pname = "intray-server";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec autodocodec-yaml base bytestring
    conduit containers envparse exceptions file-embed http-client
    intray-api intray-data intray-stripe-client jose markdown mergeless
    mergeless-persistent monad-logger mtl optparse-applicative path
    path-io persistent persistent-sqlite pretty-show resourcet safe
    servant servant-auth-server servant-docs servant-server text time
    typed-uuid unordered-containers wai wai-cors warp yaml
  ];
  executableHaskellDepends = [ base ];
  description = "Intray Server";
  license = "unknown";
  mainProgram = "intray-server";
}
