{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, c-storable-deriving, clock
      , containers, contravariant, deepseq, directory, distributive
      , filepath, JuicyPixels, lens, linear, managed, megaparsec, mtl
      , OpenGL, OpenGLRaw, profunctors, rapid, reactive-banana, sdl2
      , semigroupoids, stdenv, stm, text, these, transformers, vector
      , wires, monoidal-containers, either, generic-deriving, plan-applicative
      }:
      mkDerivation {
        pname = "quake3-bsp-viewer";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base bytestring c-storable-deriving clock containers contravariant
          deepseq directory distributive filepath JuicyPixels lens linear
          managed megaparsec mtl OpenGL OpenGLRaw profunctors rapid
          reactive-banana sdl2 semigroupoids stm text these transformers
          vector wires monoidal-containers either generic-deriving plan-applicative
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = (haskellPackages.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation (args // {
        enableLibraryProfiling = true;
      });
      wires = self.callPackage ./wires {};
    };
  }).callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
