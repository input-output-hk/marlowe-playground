{ repoRoot, inputs, pkgs, ... }:

let
  npmlock2nix = import inputs.npmlock2nix { inherit pkgs; };

  spagoPkgs = import (inputs.self + "/marlowe-playground-client/spago-packages.nix") { inherit pkgs; };
in

npmlock2nix.v1.build {
  src = inputs.self + "/marlowe-playground-client";
  installPhase = ''
    mkdir -p $out/share/marlowe-playground-client/
    cp -r dist $out/share/marlowe-playground-client/static
  '';
  node_modules_attrs = {
    githubSourceHashMap = {
      shmish111.nearley-webpack-loader."939360f9d1bafa9019b6ff8739495c6c9101c4a1" = "1brx669dgsryakf7my00m25xdv7a02snbwzhzgc9ylmys4p8c10x";
      ankitrohatgi.tarballjs."64ea5eb78f7fc018a223207e67f4f863fcc5d3c5" = "04r9yap0ka4y3yirg0g7xb63mq7jzc2qbgswbixxj8s60k6zdqsm";
    };
    buildInputs = [ pkgs.python ];
  };
  buildInputs = [
    spagoPkgs.installSpagoStyle
    spagoPkgs.buildSpagoStyle
    pkgs.nodejs-14_x
    repoRoot.nix.marlowe-playground.easy-purescript-nix.purs
    repoRoot.nix.marlowe-playground.easy-purescript-nix.spago2nix
  ];
  unpackPhase = ''
    mkdir -p marlowe-playground-client
    cp -r $src/* marlowe-playground-client

    mkdir -p web-common-marlowe
    cp -r ${inputs.self + "/web-common-marlowe"}/* web-common-marlowe

    mkdir -p web-common
    cp -r ${inputs.self + "/web-common"}/* web-common

    cd marlowe-playground-client
    install-spago-style
    cd ..
  '';
  buildCommands = [
    ''
      cd marlowe-playground-client
      build-spago-style \
        "./src/**/*.purs" \
        "./generated/**/*.purs" \
        "../web-common-marlowe/src/**/*.purs" \
        "../web-common/src/**/*.purs"
      rm -rf ./dist
      npm run build:webpack:prod
    ''
  ];
}
