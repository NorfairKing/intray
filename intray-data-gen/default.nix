{ mkDerivation, base, bytestring, genvalidity
, genvalidity-bytestring, genvalidity-persistent
, genvalidity-sydtest, genvalidity-sydtest-aeson, genvalidity-text
, genvalidity-time, genvalidity-typed-uuid, genvalidity-uuid
, intray-data, lib, QuickCheck, sydtest, sydtest-discover
, sydtest-persistent-sqlite, text
}:
mkDerivation {
  pname = "intray-data-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-bytestring genvalidity-persistent
    genvalidity-text genvalidity-time genvalidity-typed-uuid
    genvalidity-uuid intray-data QuickCheck text
  ];
  testHaskellDepends = [
    base bytestring genvalidity-sydtest genvalidity-sydtest-aeson
    genvalidity-text intray-data QuickCheck sydtest
    sydtest-persistent-sqlite text
  ];
  testToolDepends = [ sydtest-discover ];
  description = "Generators for intray-data";
  license = "unknown";
}
