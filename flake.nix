{

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.easy-purescript-nix = {
    url = "github:justinwoo/easy-purescript-nix";
    flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, easy-purescript-nix }:
    let
      supportedSystems = [
        # "x86_64-linux"
        "x86_64-darwin"
      ];
    in
    flake-utils.lib.eachSystem supportedSystems (system:
      let
        easyPS =
          let
            p = pkgs.callPackage (easy-purescript-nix) { };
          in
          p // { purs = p.purs-0_15_2; };

        scripts = import ./nix/scripts.nix {
          inherit pkgs system easyPS;
          inherit (pkgs.nodePackages) prettier;
        };
        overlays = [
          haskellNix.overlay
          (final: prev: {
            playground =
              final.haskell-nix.cabalProject' {
                src = ./.;
                compiler-nix-name = "ghc8107";
                shell.tools = {
                  cabal = { };
                  # hlint = {};
                  haskell-language-server = { };
                };
                shell.buildInputs =
                  (with pkgs; [
                    nixpkgs-fmt
                    nodePackages.prettier
                  ]
                  )
                  ++
                  (with scripts; [
                    marlowePlaygroundGeneratePurs
                  ]
                  )
                  ++
                  (with easyPS; [
                    purs-tidy
                    purs
                    spago
                    spago2nix
                    psa
                    purescript-language-server
                    pscid
                  ]

                  )
                ;
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.playground.flake { };
      in
      flake // {
        legacyPackages = pkgs;
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };
}
