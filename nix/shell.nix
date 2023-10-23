{ repoRoot, inputs, pkgs, system, lib }:

_cabalProject:

{
  name = "marlowe-playground";


  env = {
    PLAYWRIGHT_BROWSERS_PATH =
      lib.optionalString (system == "x86_64-linux")
        "${repoRoot.nix.playwright}";

    PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "true";
  };


  scripts.generate-purescript = {
    exec = repoRoot.nix.scripts.generate-purescript;
    description = "Generate all PureScript Bridge code";
    group = "marlowe-playground";
  };

  scripts.update-client-deps = {
    exec = repoRoot.nix.scripts.update-client-deps;
    description = "Run spago2nix in marlowe-playground-client";
    group = "marlowe-playground";
  };

  scripts.start-backend = {
    exec = repoRoot.nix.scripts.start-backend;
    description = "Start the Marlowe Playground server";
    group = "marlowe-playground";
  };

  scripts.gen-nix-lockfiles = {
    exec = repoRoot.nix.scripts.gen-nix-lockfiles;
    group = "marlowe-playground";
    description = "Run this whenever package-json.lock or packages.dhall change";
  };


  packages = [
    repoRoot.nix.easy-purescript-nix.purs-tidy
    repoRoot.nix.easy-purescript-nix.purs
    repoRoot.nix.easy-purescript-nix.spago
    repoRoot.nix.easy-purescript-nix.spago2nix
    repoRoot.nix.easy-purescript-nix.psa
    repoRoot.nix.easy-purescript-nix.purescript-language-server
    repoRoot.nix.easy-purescript-nix.pscid

    pkgs.jq
    pkgs.yq
    pkgs.gnused
    pkgs.act
    pkgs.gawk
    pkgs.nil
    pkgs.z3
    pkgs.which
    pkgs.python38
    pkgs.prefetch-npm-deps
    pkgs.nodejs-18_x
    pkgs.nodejs-18_x.pkgs.webpack
    pkgs.nodejs-18_x.pkgs.webpack-cli
  ];

  preCommit = {
    shellcheck.enable = true;
    stylish-haskell.enable = false;
    nixpkgs-fmt.enable = true;
    prettier.enable = true;
    purs-tidy.enable = true;
  };
}
