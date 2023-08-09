{ nix, inputs', ... }:
{
  packages.generated-purescript = nix.marlowe-playground.generated-purescript;

  packages.marlowe-playground-client = nix.marlowe-playground.marlowe-playground-client;

  packages.marlowe-playground-server = inputs'.self.apps.marlowe-playground-server;

  packages.playwright = nix.marlowe-playground.playwright;
}