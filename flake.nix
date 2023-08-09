{
  description = "Marlowe Playground";


  inputs = {

    iogx.url = "github:input-output-hk/haskell.nix";

    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };

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
