{ inputs, cell }:
let
  inherit (cell) library packages scripts;
  inherit (library) pkgs haskell-nix cabal-project;
  haskell-devshell = haskell-nix.haskellLib.devshellFor cabal-project.shell;

  shell = inputs.std.lib.dev.mkShell {
    name = "marlowe-playground-shell";

    imports = [ haskell-devshell ];

    commands = [
      {
        package = scripts.fix-png-optimization;
        category = "general commands";
        help = "Optimize all PNG files in-place";
      }
      {
        package = scripts.fix-stylish-haskell;
        category = "haskell";
        help = "Format all Haskell files in-place";
      }
      {
        package = scripts.fix-prettier;
        category = "javascript";
        help = "Format all JavaScript, HTML, and CSS files in-place";
      }
      {
        package = scripts.fix-purs-tidy;
        category = "purescript";
        help = "Format all PureScript files in-place";
      }
      {
        package = scripts.fix-nix-fmt;
        category = "nix";
        help = "Format all Nix files in-place";
      }
      {
        package = pkgs.shellcheck;
        category = "general commands";
        help = "Shell file checker";
      }
      {
        package = pkgs.editorconfig-checker;
        category = "general commands";
        help = "Editorconfig compliance checker";
      }
      {
        package = library.cabal-install;
        name = "cabal";
        category = "haskell";
        help = "Cabal build tool";
      }
      {
        package = library.hlint;
        name = "hlint";
        category = "haskell";
        help = "Haskell code linter";
      }
      {
        package = library.hlint;
        name = "hlint";
        category = "haskell";
        help = "Haskell code linter";
      }
      {
        package = library.stylish-haskell;
        name = "stylish-haskell";
        category = "haskell";
        help = "Haskell code formatter";
      }
      {
        package = packages.nixpkgs-fmt;
        category = "nix";
        help = "Nix code formatter";
      }
      {
        package = pkgs.nodePackages.prettier;
        category = "javascript";
        help = "JS, HTML, CSS code formatter";
      }
      {
        package = library.nodejs;
        category = "javascript";
        help = "NodeJS runtime";
      }
      {
        package = library.purescript.purs-tidy;
        category = "purescript";
        help = "PureScript code formatter";
      }
      {
        package = library.purescript.purs;
        category = "purescript";
        help = "PureScript compiler";
      }
      {
        package = library.purescript.spago;
        category = "purescript";
        help = "Purescript build tool";
      }
      {
        package = library.purescript.spago2nix;
        category = "purescript";
        help = "Dependency translator for Spago to Nix";
      }
      {
        package = library.purescript.psa;
        category = "purescript";
        help = "Purescript build tool";
      }
      {
        package = library.purescript.purescript-language-server;
        category = "purescript";
        help = "Purescript language server";
      }
      {
        package = library.purescript.pscid;
        category = "purescript";
        help = "Purescript development daemon";
      }
      {
        package = scripts.generate-purescript;
        category = "general commands";
        help = "Generate all PureScript Bridge code";
      }
      {
        package = scripts.start-backend;
        category = "general commands";
        help = "Start the Marlowe Playground server";
      }
      {
        package = scripts.update-client-deps;
        category = "general commands";
        help = "Run spago2nix in marlowe-playground-client";
      }
    ];

    packages = [
      library.hie-bios
      pkgs.jq
      pkgs.yq
      pkgs.pre-commit
      pkgs.gnused
      pkgs.act
      pkgs.gawk
      pkgs.nil
      pkgs.z3
    ];

    devshell.startup."pre-commit-check".text = cell.library.pre-commit-check.shellHook;
  };

in
{ default = shell; }
