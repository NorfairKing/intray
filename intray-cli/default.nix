{ mkDerivation, aeson, aeson-pretty, async, autodocodec
, autodocodec-yaml, base, bytestring, containers, cookie, envparse
, filelock, genvalidity-path, genvalidity-sydtest
, genvalidity-sydtest-aeson, http-client, http-client-tls
, http-types, intray-api, intray-api-gen, intray-cli-data
, intray-client, intray-data, intray-server-gen, lib, mergeless
, mergeless-persistent, monad-logger, mtl, network-uri
, optparse-applicative, path, path-io, persistent
, persistent-sqlite, pretty-relative-time, servant
, servant-auth-client, servant-auth-server, servant-client
, servant-server, sydtest, sydtest-discover
, sydtest-persistent-sqlite, sydtest-wai, text, time, typed-process
, unliftio, validity, yaml
}:
mkDerivation {
  pname = "intray-cli";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec autodocodec-yaml base bytestring
    containers cookie envparse filelock http-client http-client-tls
    intray-api intray-cli-data intray-client intray-data mergeless
    mergeless-persistent monad-logger mtl network-uri
    optparse-applicative path path-io persistent persistent-sqlite
    pretty-relative-time servant servant-auth-client
    servant-auth-server servant-client servant-server text time
    typed-process unliftio validity yaml
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    async base bytestring filelock genvalidity-path genvalidity-sydtest
    genvalidity-sydtest-aeson http-client http-types intray-api
    intray-api-gen intray-cli-data intray-client intray-data
    intray-server-gen mergeless monad-logger mtl path path-io servant
    servant-client sydtest sydtest-persistent-sqlite sydtest-wai text
    unliftio
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
  mainProgram = "intray";
}
