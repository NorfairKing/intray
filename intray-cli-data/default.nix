{ mkDerivation, aeson, autodocodec, base, intray-data, lib
, persistent, time
}:
mkDerivation {
  pname = "intray-cli-data";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base intray-data persistent time
  ];
  license = "unknown";
}
