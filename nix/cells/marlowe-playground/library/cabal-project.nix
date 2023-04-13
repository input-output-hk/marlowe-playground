{ inputs, cell }:

let
  inherit (cell.library) haskell-nix ghc-compiler-nix-name;
  inherit (inputs) self CHaP;

  configureProject = { pkgs, lib, ... }: {
    compiler-nix-name = ghc-compiler-nix-name;

    src = haskell-nix.haskellLib.cleanSourceWith {
      src = self.outPath;
      name = "marlowe-playground";
    };

    shell = {
      withHoogle = false;
    };

    inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP; };

    modules = [
      {
        packages.plutus-script-utils.ghcOptions = [ "-Wwarn" "-Wno-unused-packages" ];
        # See https://github.com/input-output-hk/iohk-nix/pull/488
        packages.cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
        packages.cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
      }
    ];
  };

  project = haskell-nix.cabalProject' configureProject;

in
project
