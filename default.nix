{ mkDerivation, aeson, base, binary, cereal, deepseq, hashable
, lens, log-domain, mtl, QuickCheck, semirings, stdenv, tasty
, tasty-quickcheck, tasty-th, vector, vector-th-unbox
}:
mkDerivation {
  pname = "SciBaseTypes";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base binary cereal deepseq hashable lens log-domain mtl
    semirings vector vector-th-unbox
  ];
  testHaskellDepends = [
    base QuickCheck tasty tasty-quickcheck tasty-th
  ];
  benchmarkHaskellDepends = [
    aeson base binary cereal deepseq hashable lens log-domain mtl
    semirings vector vector-th-unbox
  ];
  homepage = "https://github.com/choener/SciBaseTypes";
  description = "Base types and classes for statistics, sciences and humanities";
  license = stdenv.lib.licenses.bsd3;
}
