{ mkDerivation, base, cairo, colour, hsnoise, linear, MonadRandom
, mtl, stdenv, time, transformers
}:
mkDerivation {
  pname = "generative-art";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base cairo colour hsnoise linear MonadRandom mtl time transformers
  ];
  executableHaskellDepends = [
    base cairo colour hsnoise linear MonadRandom mtl time transformers
  ];
  testHaskellDepends = [
    base cairo colour hsnoise linear MonadRandom mtl time transformers
  ];
  homepage = "https://github.com/githubuser/generative-art#readme";
  license = stdenv.lib.licenses.bsd3;
}
