{ inputs, cell }:
let
  inherit (cell.library) pkgs purescript;
  inherit (purescript) purs-tidy;

  # just haskell sources and the stylish-haskell config file
  src = pkgs.lib.cleanSourceWith {
    src = inputs.self;
    filter = with pkgs.lib;
      name: type:
        let baseName = baseNameOf (toString name); in
        (
          baseName == ".tidyoperators" ||
          baseName == ".tidyrc.json" ||
          (type == "regular" && hasSuffix ".purs" baseName) ||
          type == "directory"
        );
  };
in
pkgs.runCommand "purs-tidy-check" { buildInputs = [ purs-tidy ]; } ''
  set +e
  cp -a ${src} orig
  cp -a ${src} purs-tidy
  chmod -R +w purs-tidy
  cd purs-tidy
  ${purs-tidy}/bin/purs-tidy format-in-place *
  cd ..
  diff --brief --recursive orig purs-tidy
  EXIT_CODE=$?
  if [[ $EXIT_CODE != 0 ]]
  then
    echo "*** purs-tidy found changes that need addressed first"
    echo "*** Please run \`nix run .#fix-purs-tidy\` and commit changes"
    exit $EXIT_CODE
  else
    echo $EXIT_CODE > $out
  fi

''
