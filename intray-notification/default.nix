{ mkDerivation, base, cookie, http-client, http-client-tls
, intray-api, intray-client, lib, opt-env-conf, opt-env-conf-test
, servant-client, sydtest, sydtest-discover, text
}:
mkDerivation {
  pname = "intray-notification";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base cookie http-client http-client-tls intray-api intray-client
    opt-env-conf servant-client text
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base opt-env-conf-test sydtest ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
  mainProgram = "intray-notification";
}
