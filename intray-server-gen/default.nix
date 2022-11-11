{ mkDerivation, base, bytestring, containers, cookie, criterion
, genvalidity-bytestring, genvalidity-containers
, genvalidity-mergeless, genvalidity-sydtest, genvalidity-text
, genvalidity-time, http-client, http-types, intray-api
, intray-api-gen, intray-client, intray-data, intray-data-gen
, intray-server, lib, mergeless, microlens, monad-logger, path
, path-io, persistent, persistent-sqlite, QuickCheck, resourcet
, servant, servant-auth-client, servant-auth-server, servant-client
, servant-server, sydtest, sydtest-aeson, sydtest-discover
, sydtest-persistent-sqlite, sydtest-wai, text, time, typed-uuid
, wai, warp
}:
mkDerivation {
  pname = "intray-server-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers cookie genvalidity-bytestring
    genvalidity-containers genvalidity-mergeless genvalidity-sydtest
    genvalidity-text genvalidity-time http-client http-types intray-api
    intray-client intray-data intray-data-gen intray-server microlens
    monad-logger path path-io persistent-sqlite QuickCheck resourcet
    servant servant-auth-client servant-auth-server servant-client
    servant-server sydtest sydtest-persistent-sqlite sydtest-wai text
    time typed-uuid wai warp
  ];
  testHaskellDepends = [
    base bytestring containers genvalidity-bytestring
    genvalidity-sydtest genvalidity-text genvalidity-time http-types
    intray-api intray-api-gen intray-client intray-data intray-data-gen
    intray-server mergeless path path-io persistent QuickCheck servant
    servant-client servant-server sydtest sydtest-aeson text time
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    base bytestring cookie criterion http-client intray-client
    QuickCheck servant servant-auth-client servant-client sydtest
  ];
  description = "Test utils for intray-server";
  license = "unknown";
}
