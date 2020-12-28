# this configuration is contrib, if it doesn't work anymore, please fix it and submit a PR
{ mkDerivation, base, base-compat, containers, deepseq, hashable
, integer-logarithms, QuickCheck, quickcheck-instances
, semigroupoids, stdenv, tagged, tasty, tasty-quickcheck
, transformers, universe-base, universe-reverse-instances
, unordered-containers
}:
mkDerivation {
  pname = "lattices";
  version = "2.0.2";
  sha256 = "108rhpax72j6xdl0yqdmg7n32l1j805861f3q9wd3jh8nc67avix";
  revision = "2";
  editedCabalFile = "085b0qn6ya4jchgk86yyvvhac1flricryibrvi4ni9kqw29dr31g";
  libraryHaskellDepends = [
    base base-compat containers deepseq hashable integer-logarithms
    QuickCheck semigroupoids tagged transformers universe-base
    universe-reverse-instances unordered-containers
  ];
  testHaskellDepends = [
    base base-compat containers QuickCheck quickcheck-instances tasty
    tasty-quickcheck transformers universe-base
    universe-reverse-instances unordered-containers
  ];
  homepage = "http://github.com/phadej/lattices/";
  description = "Fine-grained library for constructing and manipulating lattices";
  license = stdenv.lib.licenses.bsd3;
}
