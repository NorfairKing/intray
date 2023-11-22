{ mkDerivation, base, bytestring, containers, cookie, criterion
, genvalidity-bytestring, genvalidity-containers
, genvalidity-mergeless, genvalidity-sydtest, genvalidity-text
, genvalidity-time, http-client, http-types, intray-api
, intray-api-gen, intray-client, intray-data-gen, intray-server
, lib, monad-logger, path, path-io, persistent, persistent-sqlite
, QuickCheck, servant-auth-server, servant-client, servant-server
, sydtest, sydtest-aeson, sydtest-discover
, sydtest-persistent-sqlite, sydtest-wai, text, typed-uuid
}:
mkDerivation {
  pname = "intray-server-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers cookie genvalidity-bytestring
    genvalidity-containers genvalidity-mergeless genvalidity-sydtest
    genvalidity-text genvalidity-time http-client http-types intray-api
    intray-client intray-data-gen intray-server monad-logger path
    path-io persistent-sqlite QuickCheck servant-auth-server
    servant-client servant-server sydtest sydtest-persistent-sqlite
    sydtest-wai text typed-uuid
  ];
  testHaskellDepends = [
    base bytestring containers genvalidity-bytestring
    genvalidity-sydtest genvalidity-text genvalidity-time http-types
    intray-api intray-api-gen intray-client intray-data-gen
    intray-server path path-io persistent QuickCheck sydtest
    sydtest-aeson text
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    base bytestring criterion http-client intray-client sydtest
  ];
  description = "Test utils for intray-server";
  license = "unknown";
}
