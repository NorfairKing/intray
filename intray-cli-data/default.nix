{ mkDerivation, aeson, autodocodec, base, bytestring, intray-api
, lib, persistent, text, time
}:
mkDerivation {
  pname = "intray-cli-data";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base bytestring intray-api persistent text time
  ];
  license = "unknown";
}
