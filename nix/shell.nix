# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#34-nixshellnix

{ repoRoot, inputs', pkgs, lib, system, ... }:

{
  name = "marlowe-playground";


  env = {
    PLAYWRIGHT_BROWSERS_PATH =
      lib.optionalString (system == "x86_64-linux")
        "${repoRoot.nix.marlowe-playground.playwright}";

    PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "true";
  };


  scripts.generate-purescript = {
    exec = repoRoot.nix.marlowe-playground.scripts.generate-purescript;
    description = "Generate all PureScript Bridge code";
    group = "marlowe-playground";
  };

  scripts.update-client-deps = {
    exec = repoRoot.nix.marlowe-playground.scripts.update-client-deps;
    description = "Run spago2nix in marlowe-playground-client";
    group = "marlowe-playground";
  };

  scripts.start-backend = {
    exec = repoRoot.nix.marlowe-playground.scripts.start-backend;
    description = "Start the Marlowe Playground server";
    group = "marlowe-playground";
  };


  packages = [
    repoRoot.nix.marlowe-playground.easy-purescript-nix.purs-tidy
    repoRoot.nix.marlowe-playground.easy-purescript-nix.purs
    repoRoot.nix.marlowe-playground.easy-purescript-nix.spago
    repoRoot.nix.marlowe-playground.easy-purescript-nix.spago2nix
    repoRoot.nix.marlowe-playground.easy-purescript-nix.psa
    repoRoot.nix.marlowe-playground.easy-purescript-nix.purescript-language-server
    repoRoot.nix.marlowe-playground.easy-purescript-nix.pscid

    pkgs.jq
    pkgs.yq
    pkgs.gnused
    pkgs.act
    pkgs.gawk
    pkgs.nil
    pkgs.z3
    pkgs.nodejs-16_x
  ];

}
