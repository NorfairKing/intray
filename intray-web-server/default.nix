{ mkDerivation, aeson, aeson-pretty, async, autodocodec
, autodocodec-yaml, base, base16-bytestring, bytestring, containers
, cookie, data-default, envparse, genvalidity-sydtest, http-client
, http-client-tls, http-media, http-types, intray-api
, intray-client, intray-data, intray-data-gen, intray-server
, intray-server-gen, lib, monad-logger, mtl, optparse-applicative
, path, path-io, persistent, persistent-sqlite, persistent-template
, pretty-relative-time, pretty-show, QuickCheck, servant
, servant-auth-client, servant-client, shakespeare, sydtest
, sydtest-discover, sydtest-persistent-sqlite, sydtest-wai
, sydtest-yesod, template-haskell, text, time, transformers
, typed-uuid, unordered-containers, wai, yaml, yesod, yesod-auth
, yesod-autoreload, yesod-core, yesod-form, yesod-static
, yesod-static-remote
}:
mkDerivation {
  pname = "intray-web-server";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty async autodocodec autodocodec-yaml base
    base16-bytestring bytestring containers cookie data-default
    envparse http-client http-client-tls http-media http-types
    intray-api intray-client intray-data intray-server monad-logger mtl
    optparse-applicative path path-io persistent persistent-sqlite
    persistent-template pretty-relative-time pretty-show servant
    servant-auth-client servant-client shakespeare template-haskell
    text time transformers typed-uuid unordered-containers wai yaml
    yesod yesod-auth yesod-autoreload yesod-core yesod-form
    yesod-static yesod-static-remote
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base genvalidity-sydtest http-client http-types intray-client
    intray-data intray-data-gen intray-server intray-server-gen
    monad-logger mtl path path-io persistent persistent-sqlite
    QuickCheck servant-client sydtest sydtest-persistent-sqlite
    sydtest-wai sydtest-yesod text time typed-uuid yesod-auth
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
  mainProgram = "intray-web-server";
}
