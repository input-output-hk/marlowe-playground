{ inputs, cell }:
let
  inherit (cell.library) nodejs-pkgs npmlock2nix pkgs purescript;
  inherit (nodejs-pkgs) npm;
  spagoPkgs = import ../../../marlowe-playground-client/spago-packages.nix { inherit pkgs; };
in
npmlock2nix.v1.build {
  src = inputs.self + "/marlowe-playground-client";
  installPhase = "cp -r dist $out";
  buildInputs = [
    spagoPkgs.installSpagoStyle
    spagoPkgs.buildSpagoStyle
    npm
    purescript.spago2nix
    purescript.purs
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
  checkPhase = ''

  '';
  buildCommands = [
    ''
      cd marlowe-playground-client
      build-spago-style \
        "./src/**/*.purs" \
        "./test/**/*.purs" \
        "./generated/**/*.purs" \
        "../web-common-marlowe/src/**/*.purs" \
        "../web-common/src/**/*.purs"
      npm test
      rm -rf ./dist
      npm run build:webpack:prod
    ''
  ];
}

