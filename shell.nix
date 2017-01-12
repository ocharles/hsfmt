{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, ghc-exactprint, stdenv, text
      , transformers, wl-pprint-text
      }:
      mkDerivation {
        pname = "hsfmt";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base ghc-exactprint text transformers wl-pprint-text
        ];
        homepage = "https://github.com/ocharles/hsfmt";
        description = "A Haskell source code formatter";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
