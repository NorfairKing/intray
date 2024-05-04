{ mkDerivation, aeson, autodocodec, base, bytestring, intray-api
, lib, persistent, time
}:
mkDerivation {
  pname = "intray-cli-data";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base bytestring intray-api persistent time
  ];
  license = "unknown";
}
