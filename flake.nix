# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#31-flakenix

{
  description = "Marlowe Playground";


  inputs = {

    iogx.url = "github:input-output-hk/iogx";
    iogx.inputs.nixpkgs.follows = "nixpkgs";

    nixpkgs.url = "github:NixOS/nixpkgs/9952d6bc395f5841262b006fbace8dd7e143b634";

    n2c.url = "github:nlewo/nix2container";

    npmlock2nix = {
      url = "github:nix-community/npmlock2nix";
      flake = false;
    };
  };


  outputs = inputs: inputs.iogx.lib.mkFlake {
    inherit inputs;
    repoRoot = ./.;
  };


  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}
