{ nixpkgs ? import <nixpkgs> {} }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, ghc-exactprint, stdenv, text
      , transformers, wl-pprint-text, mtl
      }:
      mkDerivation {
        pname = "hsfmt";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base ghc-exactprint mtl text transformers wl-pprint-text
        ];
        homepage = "https://github.com/ocharles/hsfmt";
        description = "A Haskell source code formatter";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      wl-pprint-text = pkgs.haskell.lib.overrideCabal super.wl-pprint-text (drv: {
        src = pkgs.fetchgit {
          url = git://github.com/ocharles/wl-pprint-text;
          sha256 = "04dz1m1pvhizzi92p45kzns15isa1xs8gv7vazmnf36vl2bwgclj";
          rev = "e781145ac9546991a9b482d3f3fb1169ad93927a";
        };
        buildDepends = [ super.base-compat ];
      });
    };
  };

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
