# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#31-flakenix

{
  description = "Marlowe Playground";


  inputs = {

    iogx.url = "github:input-output-hk/iogx";

    n2c.url = "github:nlewo/nix2container";

    npmlock2nix = {
      url = "github:nix-community/npmlock2nix";
      flake = false;
    };

    std.url = "github:divnix/std";
    std.inputs.n2c.follows = "n2c";
  };


  outputs = inputs: inputs.iogx.lib.mkFlake {
    inherit inputs;
    repoRoot = ./.;
    nixpkgsConfig = {
      permittedInsecurePackages = [
        "python-2.7.18.6"
        "nodejs-14.21.3"
        "openssl-1.1.1u"
      ];
    };
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
