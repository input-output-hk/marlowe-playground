# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#35-nixper-system-outputsnix

{ nix, projects, ... }:

{
  packages.generated-purescript = nix.marlowe-playground.generated-purescript;

  packages.marlowe-playground-client = nix.marlowe-playground.marlowe-playground-client;

  packages.playwright = nix.marlowe-playground.playwright;

  operables = nix.marlowe-cardano.operables projects.default;

  oci-images = nix.marlowe-cardano.oci-images;
}