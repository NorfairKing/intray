{ mkDerivation, base, bytestring, genvalidity
, genvalidity-bytestring, genvalidity-sydtest
, genvalidity-sydtest-aeson, genvalidity-text, genvalidity-time
, genvalidity-uuid, intray-api, intray-data-gen, lib, QuickCheck
, sydtest, sydtest-discover, text
}:
mkDerivation {
  pname = "intray-api-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring genvalidity genvalidity-bytestring genvalidity-text
    genvalidity-time genvalidity-uuid intray-api intray-data-gen
    QuickCheck text
  ];
  testHaskellDepends = [
    base bytestring genvalidity-sydtest genvalidity-sydtest-aeson
    genvalidity-text intray-api QuickCheck sydtest text
  ];
  testToolDepends = [ sydtest-discover ];
  description = "Generators for intray-api";
  license = "unknown";
}
