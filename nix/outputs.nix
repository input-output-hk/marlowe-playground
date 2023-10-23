{ repoRoot, inputs, pkgs, system, lib }:
let
  project = repoRoot.nix.project;
in
[
  (
    project.flake
  )

  {
    packages.generated-purescript = repoRoot.nix.generated-purescript;
    packages.marlowe-playground-client = repoRoot.nix.marlowe-playground-client;
    packages.ghc-with-marlowe = project.cabalProject.ghcWithPackages (pkgs: [ pkgs.marlowe-cardano ]);

    playwright = repoRoot.nix.playwright;
    operables = repoRoot.nix.operables;
    oci-images = repoRoot.nix.oci-images;

    hydraJobs.operables = repoRoot.nix.operables;
    hydraJobs.oci-images = repoRoot.nix.oci-images;
  }

  (lib.optionalAttrs (system == "x86_64-linux") {
    hydraJobs.playwright = repoRoot.nix.playwright;
  })
]
