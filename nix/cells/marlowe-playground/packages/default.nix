{ inputs, cell }@block:
{
  flow-chart-generator = import ./flow-chart-generator.nix block;

  generated-purescript = import ./generated-purescript.nix block;

  ghc-with-marlowe = import ./ghc-with-marlowe.nix block;

  marlowe-playground-client = import ./marlowe-playground-client.nix block;

  marlowe-playground-server = import ./marlowe-playground-server.nix block;

  nixpkgs-fmt = import ./nixpkgs-fmt.nix block;
}
