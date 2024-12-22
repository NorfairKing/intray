{ mkDerivation, autodocodec, base, bytestring, containers, cookie
, filelock, genvalidity-path, genvalidity-sydtest
, genvalidity-sydtest-aeson, http-client, http-client-tls
, intray-api, intray-cli-data, intray-client, lib, mergeless
, mergeless-persistent, monad-logger, mtl, network-uri
, opt-env-conf, opt-env-conf-test, path, path-io, persistent
, persistent-sqlite, pretty-relative-time, servant-auth-client
, servant-client, sydtest, sydtest-discover, sydtest-wai, text
, time, typed-process
}:
mkDerivation {
  pname = "intray-cli";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    autodocodec base bytestring containers cookie filelock http-client
    http-client-tls intray-api intray-cli-data intray-client mergeless
    mergeless-persistent monad-logger mtl network-uri opt-env-conf path
    path-io persistent persistent-sqlite pretty-relative-time
    servant-auth-client servant-client text time typed-process
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring genvalidity-path genvalidity-sydtest
    genvalidity-sydtest-aeson http-client intray-api monad-logger mtl
    opt-env-conf-test path path-io sydtest sydtest-wai text
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
  mainProgram = "intray";
}
