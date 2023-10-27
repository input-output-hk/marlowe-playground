{ repoRoot, inputs, pkgs, system, lib }:

let
  spago-pkgs = import ./spago-packages.nix { inherit pkgs; };
in

pkgs.buildNpmPackage {

  pname = "marlowe-playground-client";

  version = "0.1.0";

  src = lib.sourceByRegex (inputs.self + /marlowe-playground-client) [
    "^prototype.*"
    "^generated.*"
    "^src.*"
    "^static.*"
    "^test.*"
    "^entry.*"
    "^grammar.ne$"
    "^package.*"
    "^postcss.*"
    "^spago.*"
    "^tailwind.*"
    "^tsconfig.*"
    "^webpack.*"
  ];

  dontNpmBuild = true;
  dontNpmInstall = true;

  # buildNpmPackage is able to make a pure nix build by using npmDepsHash.
  # That is the hash of package-lock.json.
  # Its value is generated using the prefetch-npm-deps command (see shell.nix).
  # We set dontNpmBuild and dontNpmInstall to true to significantly speed up the 
  # build: this works because we have a custom buildPhase that invokes webpack-cli
  # explicitely.
  npmDepsHash = import ./npm-deps-hash.nix;

  nativeBuildInputs = [
    spago-pkgs.installSpagoStyle
    spago-pkgs.buildSpagoStyle
    repoRoot.nix.easy-purescript-nix.purs
    repoRoot.nix.easy-purescript-nix.spago2nix
    pkgs.nodejs_14
    pkgs.nodejs_14.pkgs.webpack
    pkgs.nodejs_14.pkgs.webpack-cli
  ];

  buildPhase = ''
    mkdir -p {marlowe-playground-client,web-common-marlowe,web-common}

    cp -r $src/* marlowe-playground-client
    cp -r ${inputs.self + "/web-common-marlowe"}/* web-common-marlowe
    cp -r ${inputs.self + "/web-common"}/* web-common

    cd marlowe-playground-client 
    
    install-spago-style

    build-spago-style \
      "./src/**/*.purs" \
      "./generated/**/*.purs" \
      "../web-common-marlowe/src/**/*.purs" \
      "../web-common/src/**/*.purs"

    npm run build:webpack:prod

    mkdir -p $out/share/marlowe-playground-client/static

    cp -r dist $out/share/marlowe-playground-client/static
  '';
}
