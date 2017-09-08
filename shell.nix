{ nixpkgs ? import <nixpkgs> {} }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, ghc-exactprint, stdenv, text
      , transformers, prettyprinter, mtl, hedgehog
      }:
      mkDerivation {
        pname = "hsfmt";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base ghc-exactprint mtl text transformers prettyprinter
          hedgehog
        ];
        homepage = "https://github.com/ocharles/hsfmt";
        description = "A Haskell source code formatter";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
    };
  };

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
