# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#34-nixshellnix

{ nix, inputs', pkgs, ... }:

{
  name = "marlowe-playground";


  env = {
    PLAYWRIGHT_BROWSERS_PATH = "${nix.marlowe-playground.playwright}"; 
    PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "true"; 
  };


  scripts.generate-purescript = {
    exec = nix.marlowe-playground.scripts.generate-purescript;
    description = "Generate all PureScript Bridge code";
    group = "purescript";
  };

  scripts.update-client-deps = {
    exec = nix.marlowe-playground.scripts.update-client-deps;
    description = "Run spago2nix in marlowe-playground-client";
    group = "purescript";
  };

  scripts.start-backend = {
    exec = nix.marlowe-playground.scripts.start-backend;
    description = "Start the Marlowe Playground server";
    group = "general";
  };


  packages = [
    inputs'.iogx.inputs.easy-purescript-nix.purs-tidy
    inputs'.iogx.inputs.easy-purescript-nix.purs
    inputs'.iogx.inputs.easy-purescript-nix.spago
    inputs'.iogx.inputs.easy-purescript-nix.spago2nix
    inputs'.iogx.inputs.easy-purescript-nix.psa
    inputs'.iogx.inputs.easy-purescript-nix.purescript-language-server
    inputs'.iogx.inputs.easy-purescript-nix.pscid

    pkgs.jq
    pkgs.yq
    pkgs.gnused
    pkgs.act
    pkgs.gawk
    pkgs.nil
    pkgs.z3
    pkgs.nodejs-14_x
  ];

}