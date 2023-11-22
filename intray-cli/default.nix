{ mkDerivation, autodocodec, autodocodec-yaml, base, bytestring
, containers, cookie, envparse, filelock, genvalidity-path
, genvalidity-sydtest, genvalidity-sydtest-aeson, http-client
, http-client-tls, intray-api, intray-api-gen, intray-cli-data
, intray-client, intray-data, intray-server-gen, lib, mergeless
, mergeless-persistent, monad-logger, mtl, network-uri
, optparse-applicative, path, path-io, persistent
, persistent-sqlite, pretty-relative-time, servant-auth-client
, servant-client, sydtest, sydtest-discover, sydtest-wai, text
, time, typed-process, unliftio, validity, yaml
}:
mkDerivation {
  pname = "intray-cli";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    autodocodec autodocodec-yaml base bytestring containers cookie
    envparse filelock http-client http-client-tls intray-api
    intray-cli-data intray-client intray-data mergeless
    mergeless-persistent monad-logger mtl network-uri
    optparse-applicative path path-io persistent persistent-sqlite
    pretty-relative-time servant-auth-client servant-client text time
    typed-process validity yaml
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring filelock genvalidity-path genvalidity-sydtest
    genvalidity-sydtest-aeson http-client intray-api-gen intray-client
    intray-data intray-server-gen monad-logger mtl path path-io sydtest
    sydtest-wai text unliftio
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
  mainProgram = "intray";
}
