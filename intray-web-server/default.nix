{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base
, containers, cookie, data-default, envparse, genvalidity-sydtest
, http-client, http-client-tls, http-types, intray-api
, intray-client, intray-data, intray-data-gen, intray-server-gen
, lib, monad-logger, mtl, optparse-applicative, path, path-io
, persistent, pretty-relative-time, pretty-show, QuickCheck
, servant-auth-client, servant-client, shakespeare, sydtest
, sydtest-discover, sydtest-yesod, template-haskell, text, time
, typed-uuid, yesod, yesod-auth, yesod-autoreload, yesod-static
, yesod-static-remote
}:
mkDerivation {
  pname = "intray-web-server";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-yaml base containers cookie
    data-default envparse http-client http-client-tls http-types
    intray-api intray-client monad-logger mtl optparse-applicative path
    path-io persistent pretty-relative-time pretty-show
    servant-auth-client servant-client shakespeare template-haskell
    text time typed-uuid yesod yesod-auth yesod-autoreload yesod-static
    yesod-static-remote
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base containers genvalidity-sydtest http-client http-types
    intray-client intray-data intray-data-gen intray-server-gen mtl
    path path-io QuickCheck servant-client sydtest sydtest-yesod text
    time typed-uuid yesod-auth
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
  mainProgram = "intray-web-server";
}
