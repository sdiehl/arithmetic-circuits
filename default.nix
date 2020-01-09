{ mkDerivation, aeson, base, bulletproofs, containers, criterion
, elliptic-curve, filepath, galois-fft, galois-field, hpack
, markdown-unlit, MonadRandom, pairing, poly, process-extras
, protolude, QuickCheck, quickcheck-instances, semirings, stdenv
, tasty, tasty-discover, tasty-hunit, tasty-quickcheck, text
, vector, wl-pprint-text
}:
mkDerivation {
  pname = "arithmetic-circuits";
  version = "0.2.0";
  src = ./arithmetic-circuits;
  libraryHaskellDepends = [
    aeson base bulletproofs containers elliptic-curve filepath
    galois-fft galois-field MonadRandom poly process-extras protolude
    semirings text vector wl-pprint-text
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson base bulletproofs containers elliptic-curve filepath
    galois-fft galois-field markdown-unlit MonadRandom pairing poly
    process-extras protolude QuickCheck quickcheck-instances semirings
    tasty tasty-discover tasty-hunit tasty-quickcheck text vector
    wl-pprint-text
  ];
  testToolDepends = [ markdown-unlit tasty-discover ];
  benchmarkHaskellDepends = [
    aeson base bulletproofs containers criterion elliptic-curve
    filepath galois-fft galois-field MonadRandom pairing poly
    process-extras protolude semirings text vector wl-pprint-text
  ];
  prePatch = "hpack";
  homepage = "https://github.com/adjoint-io/arithmetic-circuits#readme";
  description = "Arithmetic circuits for zkSNARKs";
  license = stdenv.lib.licenses.mit;
}
