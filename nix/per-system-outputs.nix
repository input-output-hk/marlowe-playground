# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#35-nixper-system-outputsnix

{ repoRoot, haskellProjects, ... }:

{
  packages.generated-purescript = repoRoot.nix.marlowe-playground.generated-purescript;

  packages.marlowe-playground-client = repoRoot.nix.marlowe-playground.marlowe-playground-client;

  packages.playwright = repoRoot.nix.marlowe-playground.playwright;

  packages.ghc-with-marlowe = haskellProjects.default.ghcWithPackages (pkgs: [ pkgs.marlowe-cardano ]);

  operables = repoRoot.nix.marlowe-playground.operables haskellProjects.default;

  oci-images = repoRoot.nix.marlowe-playground.oci-images;
}