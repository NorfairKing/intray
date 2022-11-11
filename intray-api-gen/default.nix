{ mkDerivation, base, bytestring, genvalidity
, genvalidity-bytestring, genvalidity-mergeless
, genvalidity-sydtest, genvalidity-sydtest-aeson, genvalidity-text
, genvalidity-time, genvalidity-uuid, intray-api, intray-data
, intray-data-gen, lib, QuickCheck, sydtest, sydtest-discover, text
, validity, validity-bytestring, validity-text, validity-time
}:
mkDerivation {
  pname = "intray-api-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring genvalidity genvalidity-bytestring
    genvalidity-mergeless genvalidity-text genvalidity-time
    genvalidity-uuid intray-api intray-data intray-data-gen QuickCheck
    text validity validity-bytestring validity-text validity-time
  ];
  testHaskellDepends = [
    base bytestring genvalidity-sydtest genvalidity-sydtest-aeson
    genvalidity-text intray-api intray-data intray-data-gen QuickCheck
    sydtest text
  ];
  testToolDepends = [ sydtest-discover ];
  description = "Generators for intray-api";
  license = "unknown";
}
