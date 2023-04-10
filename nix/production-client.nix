{ pkgs, clientSrc, inputs, spagoPkgs, easyPS, webCommon, webCommonMarlowe }:
let
  nodePkgs = pkgs.nodejs-14_x.pkgs;
  npm2nix = import inputs.npmlock2nix { inherit pkgs; };
in
npm2nix.v1.build {
  src = clientSrc;
  installPhase = "cp -r dist $out";
  buildInputs = [
    spagoPkgs.installSpagoStyle
    spagoPkgs.buildSpagoStyle
    nodePkgs.npm
    easyPS.spago2nix
    easyPS.purs
  ];
  unpackPhase = ''
    mkdir -p marlowe-playground-client
    cp -r $src/* marlowe-playground-client

    mkdir -p web-common-marlowe
    cp -r ${webCommonMarlowe}/* web-common-marlowe

    mkdir -p web-common
    cp -r ${webCommon}/* web-common

    cd marlowe-playground-client
    install-spago-style
    cd ..
  '';
  buildCommands = [
    ''
      cd marlowe-playground-client
      build-spago-style "./src/**/*.purs" "./generated/**/*.purs" "../web-common-marlowe/src/**/*.purs" "../web-common/src/**/*.purs"
      npm run build:webpack:prod
    ''
  ];
}

