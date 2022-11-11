{ mkDerivation, aeson, aeson-pretty, autodocodec, autodocodec-yaml
, base, bytestring, containers, cookie, envparse, filelock
, http-client, http-client-tls, intray-data, lib, mergeless
, mergeless-persistent, monad-logger, mtl, network-uri
, optparse-applicative, path, path-io, persistent
, persistent-sqlite, pretty-relative-time, servant
, servant-auth-client, servant-auth-server, servant-client
, servant-server, text, time, typed-process, unliftio, validity
, yaml
}:
mkDerivation {
  pname = "intray-cli-data";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec autodocodec-yaml base bytestring
    containers cookie envparse filelock http-client http-client-tls
    intray-data mergeless mergeless-persistent monad-logger mtl
    network-uri optparse-applicative path path-io persistent
    persistent-sqlite pretty-relative-time servant servant-auth-client
    servant-auth-server servant-client servant-server text time
    typed-process unliftio validity yaml
  ];
  license = "unknown";
}
