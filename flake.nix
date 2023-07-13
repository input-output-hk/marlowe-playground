{
  description = "Marlowe Playground";
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs";
      follows = "haskell-nix/nixpkgs-unstable";
    };
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };
    haskell-language-server = {
      url = "github:haskell/haskell-language-server?ref=1.3.0";
      flake = false;
    };
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    };
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    };
    std = {
      url = "github:divnix/std";
      inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
      inputs.devshell.follows = "devshell";
      inputs.n2c.follows = "n2c";
    };
    devshell.url = "github:numtide/devshell";
    n2c.url = "github:nlewo/nix2container";
    npmlock2nix = {
      url = "github:nix-community/npmlock2nix";
      flake = false;
    };
  };

  # Managed by std
  outputs = inputs:

    # Produces the flake outputs.
    inputs.std.growOn

      # The first argument tells std where to look for "cells", and what type
      # of cells to grow.
      {
        inherit inputs;

        # All nix files reside in this folder. Each subfolder is a "cell",
        # which is a logical grouping of functionality.
        #
        # Our two cells are:
        #   automation
        #     CI jobs and dev shells.
        #   marlowe-playground
        #     Backend and frontend for the Marlowe Playground.
        cellsFrom = ./nix/cells;

        # Each cell contains "cell blocks".
        # Block names are arbitrary.
        # Each block can be thought of as providing a "feature" to its cell.
        # Cell blocks have types.
        # Each cell block must be either:
        #   A nix file named after the cell block
        #   A directory named after the cell block and containing a default.nix
        # Not all cells have the same cell blocks.
        # All cell blocks belong in a cell.
        #
        # In this repository we have six cell blocks, listed below with their type:
        #   devshells :: devshells
        #     Development shells available via nix develop
        #   packages :: installables
        #     Derivations available via nix build
        #   library :: functions
        #     Everything that is not a derivation goes here
        #     Includes functions, attrsets and simple literal values shared across cells
        #     These are not exposed to the flake
        #   scripts :: functions
        #     Scripts that are available in the dev shell
        #   tests :: functions
        #     Scripts that are available in the dev shell
        #   ciJobs :: installables
        #     Jobsets for our Hydra CI
        #
        # std provides a TUI to interact with the cell blocks.
        # Available interactions are determined by the cell block's type.
        cellBlocks = [
          (inputs.std.blockTypes.devshells "devshells")
          (inputs.std.blockTypes.installables "packages")
          (inputs.std.blockTypes.functions "library")
          (inputs.std.blockTypes.installables "scripts")
          (inputs.std.blockTypes.installables "tests")
          (inputs.std.blockTypes.installables "ciJobs")
          (inputs.std.blockTypes.runnables "operables")
          (inputs.std.blockTypes.containers "oci-images")
        ];
      }

      # The growOn function takes a variadic number of "soil" arguments, which
      # provide a compatability layer for standard flake attributes. We
      # translate (a.k.a. "harvest") the cell blocks into flake attributes.
      # Successive arguments will be recursively merged.
      {
        # harvest the packages cell block from the marlowe-playground cell.
        packages = inputs.std.harvest inputs.self [ "marlowe-playground" "packages" ];

        # harvest the devshells cell block from the marlowe-playground cell.
        devShells = inputs.std.harvest inputs.self [ "marlowe-playground" "devshells" ];

        # harvest the tests cell block from the marlowe-playground cell.
        checks = inputs.std.harvest inputs.self [ "marlowe-playground" "tests" ];

        # harvest the ciJobs cell block from the automation cell.
        hydraJobs = inputs.std.harvest inputs.self [ "automation" "ciJobs" ];

        # harvest the oci-images cell block from the marlowe-playground cell.
        oci-images = inputs.std.harvest inputs.self [ "marlowe-playground" "oci-images" ];
      }
      {
        # harvest the scripts cell block from the marlowe-playground cell.
        packages = inputs.std.harvest inputs.self [ "marlowe-playground" "scripts" ];
      };

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = true;
  };
}
