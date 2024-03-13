{
  description = "Marlowe Playground";


  inputs = {

    iogx = {
      url = "github:input-output-hk/iogx";
      inputs.hackage.follows = "hackage";
      inputs.CHaP.follows = "CHaP";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.follows = "haskell-nix/nixpkgs";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };

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
    systems = [ "x86_64-linux" "x86_64-darwin" ];
    outputs = import ./nix/outputs.nix;
    nixpkgsArgs.config.permittedInsecurePackages = [
      "nodejs-14.21.3"
      "openssl-1.1.1w"
      "python-2.7.18.7"
    ];
    flake.nixosModules.default = import ./nix/nixos.nix inputs.self;
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
