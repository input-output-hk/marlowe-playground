{
  inputs.haskellNix = {
    url = "github:input-output-hk/haskell.nix";
    inputs.hackage.follows = "hackage";
  };
  inputs.iohkNix.url = "github:input-output-hk/iohk-nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  inputs.std.url = "github:divnix/std";
  inputs.nosys.url = "github:divnix/nosys";
  inputs.npmlock2nix = {
    url = "github:nix-community/npmlock2nix";
    flake = false;
  };
  inputs.CHaP = {
    url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
    flake = false;
  };

  inputs.hackage = {
    url = "github:input-output-hk/hackage.nix";
    flake = false;
  };

  inputs.easy-purescript-nix = {
    url = "github:justinwoo/easy-purescript-nix";
    flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, easy-purescript-nix, pre-commit-hooks, iohkNix, CHaP, std, nosys, ... }@inputs:
    let
      supportedSystems = [
        "x86_64-linux"
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

        fixPngOptimization = pkgs.callPackage ./nix/fix-png-optimization { };
        fixStylishHaskell = pkgs.callPackage ./nix/fix-stylish-haskell { };

        writeShellScriptBinInRepoRoot = name: script: pkgs.writeShellScriptBin name ''
          cd `${pkgs.git}/bin/git rev-parse --show-toplevel`
          ${script}
        '';

        scripts = import ./nix/scripts.nix {
          inherit pkgs easyPS writeShellScriptBinInRepoRoot;
          inherit (pkgs.nodePackages) prettier;

        };

        formatting = import ./nix/formatting.nix {
          inherit writeShellScriptBinInRepoRoot pkgs easyPS;
        };

        tests = import ./nix/tests/default.nix {
          inherit pkgs fixPngOptimization fixStylishHaskell;
          inherit (formatting) fix-prettier;
          inherit (easyPS) purs-tidy;
          src = ./.;
          play-generated = scripts.generated-purescript;
        };
        spagoPkgs = import ./marlowe-playground-client/spago-packages.nix { inherit pkgs; };
        production-client = import ./nix/production-client.nix {
          inherit pkgs inputs spagoPkgs easyPS;

          clientSrc = ./marlowe-playground-client;
          webCommon = ./web-common;
          webCommonMarlowe = ./web-common-marlowe;
        };
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = (pkgs.lib.cleanSource ./.);
          hooks = {
            prettier = {
              enable = true;
              types_or = [ "javascript" "css" "html" ];
            };
            purs-tidy-hook = {
              enable = true;
              name = "purs-tidy";
              description = "Ensure PureScript files are formatted";
              entry = "${easyPS.purs-tidy}/bin/purs-tidy format-in-place";
              files = "\\.purs$";
              language = "system";
            };
            shellcheck.enable = true;
            stylish-haskell.enable = true;
            nixpkgs-fmt.enable = true;
            png-optimization = {
              enable = true;
              name = "png-optimization";
              description = "Ensure that PNG files are optimized";
              entry = "${pkgs.optipng}/bin/optipng";
              files = "\\.png$";
            };
          };
        };
        overlays = [
          haskellNix.overlay
          iohkNix.overlays.crypto
          (final: prev: {
            playground =
              final.haskell-nix.cabalProject' {
                inputMap = {
                  "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
                };
                src = ./.;
                compiler-nix-name = "ghc8107";
                modules = [
                  {
                    packages.plutus-script-utils.ghcOptions = [ "-Wwarn" "-Wno-unused-packages" ];
                    # See https://github.com/input-output-hk/iohk-nix/pull/488
                    packages.cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
                    packages.cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
                  }
                ];
                shell.tools = {
                  cabal = "3.6.2.0";
                  haskell-language-server = "1.3.0.0";
                };
                shell.shellHook = ''
                  ${pre-commit-check.shellHook}

                  # The green prompt familiar to those used to nix-shell
                  export PS1="\n\[\033[1;32m\][nix develop:\w]\$\[\033[0m\] "
                '';
                shell.buildInputs =
                  (with pkgs; [
                    nixpkgs-fmt
                    nodePackages.prettier
                    pkgs.nil
                    pkgs.nodejs-14_x
                    pkgs.z3
                  ]
                  )
                  ++
                  (with scripts; [
                    marlowe-playground-generate-purs
                    start-backend
                    update-client-deps
                  ]
                  )
                  ++
                  (with formatting; [
                    fix-prettier
                    fix-purs-tidy
                    fix-nix-fmt
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
                  ++ [
                    fixPngOptimization
                    fixStylishHaskell
                  ]

                  )
                ;
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.playground.flake { };
        ghc-with-marlowe = pkgs.playground.ghcWithPackages (ps: [ ps.marlowe ]);
      in
      pkgs.lib.recursiveUpdate
        (flake // {
          legacyPackages = pkgs;
        })
        {
          packages = {
            inherit ghc-with-marlowe fixPngOptimization fixStylishHaskell;
            inherit (scripts) marlowe-playground-generate-purs generated-purescript;
            inherit (formatting) fix-purs-tidy fix-nix-fmt;
            inherit production-client;
            test-nix-fmt = tests.nixpkgsFmt;
            test-prettier = tests.prettier;
            test-generated = tests.generated;
            test-purs-tidy = tests.pursTidy;
            test-png = tests.pngOptimization;
            test-stylish-haskell = tests.stylishHaskell;
            test-shell = tests.shellcheck;
          };
          operables = import ./deploy/operables.nix {
            inputs = nosys.lib.deSys system inputs;
          };
          oci-images = import ./deploy/oci-images.nix {
            inputs = nosys.lib.deSys system inputs;
          };
        }
    );

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
