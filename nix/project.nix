{ repoRoot, inputs, pkgs, system, lib }:

let

  cabalProject = pkgs.haskell-nix.cabalProject' ({ config, pkgs, ... }: {
    name = "marlowe-playground";

    src = ../.;

    compiler-nix-name = "ghc8107";

    shell.withHoogle = false;

    inputMap = {
      "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.iogx.inputs.CHaP;
    };

    modules = [{
      packages.marlowe-symbolic.ghcOptions = [ "-Werror" ];
      packages.marlowe-playground-server.ghcOptions = [ "-Werror" ];
      packages.plutus-script-utils.ghcOptions = [ "-Wwarn" "-Wno-unused-packages" ];
    }];
  });


  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    shellArgs = repoRoot.nix.shell;
    readTheDocs = {
      enable = true;
      siteFolder = "doc";
    };
  };

in
project
