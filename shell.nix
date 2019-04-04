{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, cairo, colour, hsnoise, linear
      , MonadRandom, mtl, stdenv, time, transformers
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
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
